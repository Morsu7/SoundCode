package soundcode.ui.editor

import scalafx.scene.layout.VBox
import scalafx.geometry.Insets
import scalafx.scene.control.ScrollPane
import org.fxmisc.richtext.InlineCssTextArea
import scalafx.scene.layout.HBox
import scalafx.Includes.jfxNode2sfx
import scalafx.scene.layout.Priority
import scalafx.scene.control.Label
import scalafx.geometry.Pos
import soundcode.ui.visualizer.AnimatedView

final class BlockEditorView:
  private val LineHeight = 34

  private var firstRender = true
  private var rows = Vector.empty[BlockRow]

  private val blocksBox = new VBox:
    spacing = 2
    padding = Insets(0)
    minWidth = 0
    style = "-fx-background-color: #f4f4f5;"

  val root: ScrollPane = new ScrollPane:
    content = blocksBox
    fitToWidth = true
    hbarPolicy = ScrollPane.ScrollBarPolicy.Never
    style = "-fx-background-color: #f4f4f5;"

  root.viewportBounds.onChange {
    val width = root.viewportBounds.value.getWidth.max(0.0)
    blocksBox.prefWidth = width
  }

  def renderCode(code: String): Unit =
    if firstRender then
      buildBlocks(code)
      firstRender = false

    syncVisualizers()

  def currentCode: String =
    lineEditors.map(_.getText).mkString("\n")

  def play(): Unit =
    animatedViews.foreach(_.play())

  def stop(): Unit =
    animatedViews.foreach(_.stop())

  private def lineEditors: Vector[InlineCssTextArea] =
    rows.map(_.editor)

  private def animatedViews: Vector[AnimatedView] =
    rows.flatMap(_.visualizer)

  private def syncVisualizerAt(index: Int): Unit =
    if index >= 0 && index < rows.length then
      val row = rows(index)
      val line = row.text

      if !BlockEditorVisualizers.matches(line, row.visualizer) then
        val nextVisualizer = BlockEditorVisualizers.forLine(line)
        rows = rows.updated(
          index,
          row.withVisualizer(nextVisualizer)
        )
        nextVisualizer.foreach(v => BlockEditorAnimations.fadeIn(v.root))

  private def syncVisualizers(): Unit =
    rows.indices.foreach(syncVisualizerAt)

  private def createLineEditor(text: String): InlineCssTextArea =
    LineEditorFactory.create(
      text = text,
      lineHeight = LineHeight,
      onSplit = splitLine,
      onMergePrevious = mergeWithPreviousLine,
      onMergeNext = mergeWithNextLine,
      onMoveFocus = moveFocus,
      onFocus = clearSelectionExcept
    )

  private def buildBlocks(code: String): Unit =
    val lines = code
      .replace("\r\n", "\n")
      .replace("\r", "\n")
      .split("\n", -1)
      .toSeq

    clearBlocks()

    lines.zipWithIndex.foreach { case (line, index) =>
      addBlock(line, index)
    }

  private def clearBlocks(): Unit =
    blocksBox.children.clear()
    rows = Vector.empty

  private def addBlock(
      line: String,
      index: Int,
      existingVisualizer: Option[AnimatedView] = None
  ): Unit =
    val editor = createLineEditor(line)
    val visualizer =
      existingVisualizer.orElse(BlockEditorVisualizers.forLine(line))

    val (codeRow, lineNumberLabel) = codeLineRow(editor, index + 1)
    val node = blockNode(codeRow, visualizer)
    val row = BlockRow(editor, codeRow, node, lineNumberLabel, visualizer)

    rows = rows.patch(index, Seq(row), 0)
    blocksBox.children.add(index, node)

    if existingVisualizer.nonEmpty then BlockEditorAnimations.fadeIn(codeRow)
    else BlockEditorAnimations.fadeIn(node)

    refreshLineNumbers()

  private def removeBlockAt(
      index: Int,
      keepVisualizer: Boolean = false
  ): Unit =
    val row = rows(index)

    rows = rows.patch(index, Nil, 1)
    refreshLineNumbers()

    if keepVisualizer then blocksBox.children.remove(row.node)
    else
      row.visualizer match
        case Some(visualizer) =>
          row.node.children = Seq(visualizer.root)
          BlockEditorAnimations.fadeOut(visualizer.root) {
            blocksBox.children.remove(row.node)
          }

        case None =>
          blocksBox.children.remove(row.node)

  private def blockNode(
      codeRow: HBox,
      visualizer: Option[AnimatedView]
  ): VBox =
    new VBox:
      spacing = 0
      minWidth = 0
      children = Seq(codeRow) ++ visualizer.map(_.root).toSeq

  private def codeLineRow(
      editor: InlineCssTextArea,
      lineNumber: Int
  ): (HBox, Label) =
    val editorNode = jfxNode2sfx(editor)
    HBox.setHgrow(editorNode, Priority.Always)

    val label = new Label(lineNumber.toString):
      minWidth = LineHeight
      prefWidth = LineHeight
      maxWidth = LineHeight
      minHeight = LineHeight
      prefHeight = LineHeight
      alignment = Pos.CenterRight
      padding = Insets(0, 8, 0, 0)
      style = """
        |-fx-text-fill: #64748b;
        |-fx-background-color: #e5e7eb;
        |-fx-font-family: 'Consolas';
        |-fx-font-size: 13px;
        |""".stripMargin

    val row = new HBox:
      spacing = 0
      minWidth = 0
      minHeight = LineHeight
      maxHeight = LineHeight
      prefHeight = LineHeight
      style = "-fx-background-color: #f4f4f5;"
      children = Seq(label, editorNode)

    (row, label)

  private def splitLine(editor: InlineCssTextArea): Unit =
    val index = rows.indexWhere(_.editor eq editor)

    if index >= 0 then
      val caret = editor.getCaretPosition
      val text = editor.getText

      val before = text.take(caret)
      val after = text.drop(caret)

      val oldRow = rows(index)
      val movableVisualizer =
        if BlockEditorVisualizers.matches(after, oldRow.visualizer) then
          oldRow.visualizer
        else None

      editor.replaceText(before)
      SyntaxHighlighter.applyTo(editor)

      if movableVisualizer.nonEmpty then
        oldRow.node.children = Seq(oldRow.codeRow)
        rows = rows.updated(index, oldRow.copy(visualizer = None))
        addBlock(after, index + 1, movableVisualizer)
      else
        syncVisualizerAt(index)
        addBlock(after, index + 1)

      val nextEditor = rows(index + 1).editor
      nextEditor.requestFocus()
      nextEditor.moveTo(0)

  private def mergeWithPreviousLine(editor: InlineCssTextArea): Unit =
    val index = rows.indexWhere(_.editor eq editor)

    if index > 0 then
      val currentRow = rows(index)
      val previousRow = rows(index - 1)
      val previousEditor = previousRow.editor
      val caretPosition = previousEditor.getLength

      val mergedText = previousEditor.getText + editor.getText
      val movableVisualizer =
        if BlockEditorVisualizers.matches(mergedText, currentRow.visualizer)
        then currentRow.visualizer
        else previousRow.visualizer
      val movingCurrentVisualizer =
        currentRow.visualizer.nonEmpty &&
          movableVisualizer == currentRow.visualizer

      previousEditor.replaceText(mergedText)
      SyntaxHighlighter.applyTo(previousEditor)

      removeBlockAt(index, keepVisualizer = movingCurrentVisualizer)

      if movableVisualizer.nonEmpty then
        rows = rows.updated(
          index - 1,
          rows(index - 1).withVisualizer(movableVisualizer)
        )
      else syncVisualizerAt(index - 1)

      refreshLineNumbers()

      previousEditor.requestFocus()
      previousEditor.moveTo(caretPosition)

  private def mergeWithNextLine(editor: InlineCssTextArea): Unit =
    val index = rows.indexWhere(_.editor eq editor)

    if index >= 0 && index < rows.length - 1 then
      val currentRow = rows(index)
      val nextRow = rows(index + 1)
      val caretPosition = editor.getCaretPosition

      val mergedText = editor.getText + nextRow.editor.getText
      val movableVisualizer =
        if BlockEditorVisualizers.matches(mergedText, nextRow.visualizer) then
          nextRow.visualizer
        else currentRow.visualizer
      val movingNextVisualizer =
        nextRow.visualizer.nonEmpty &&
          movableVisualizer == nextRow.visualizer

      editor.replaceText(mergedText)
      SyntaxHighlighter.applyTo(editor)

      removeBlockAt(index + 1, keepVisualizer = movingNextVisualizer)

      if movableVisualizer != currentRow.visualizer then
        rows = rows.updated(
          index,
          rows(index).withVisualizer(movableVisualizer)
        )
      else syncVisualizerAt(index)

      refreshLineNumbers()

      editor.requestFocus()
      editor.moveTo(caretPosition)

  private def moveFocus(editor: InlineCssTextArea, delta: Int): Unit =
    val currentIndex = rows.indexWhere(_.editor eq editor)
    val nextIndex = currentIndex + delta

    if nextIndex >= 0 && nextIndex < rows.length then
      val caretColumn = editor.getCaretPosition.min(editor.getLength)
      val nextEditor = rows(nextIndex).editor
      val nextCaret = caretColumn.min(nextEditor.getLength)

      nextEditor.requestFocus()
      nextEditor.moveTo(nextCaret)

  private def refreshLineNumbers(): Unit =
    rows.zipWithIndex.foreach { case (row, index) =>
      row.lineNumberLabel.text = (index + 1).toString
    }

  private def clearSelectionExcept(activeBlock: InlineCssTextArea): Unit =
    lineEditors.filterNot(_ eq activeBlock).foreach(_.deselect())
