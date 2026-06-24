package soundcode.ui

import javafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.VBox
import scalafx.geometry.Insets
import scalafx.scene.control.ScrollPane
import org.fxmisc.richtext.InlineCssTextArea
import scalafx.scene.layout.HBox
import scalafx.Includes.jfxNode2sfx
import scalafx.scene.layout.Priority
import scalafx.scene.control.Label
import scalafx.geometry.Pos
import scala.collection.mutable.Buffer

final class BlockEditorView(
    initialCode: String,
    onCodeChanged: String => Unit
):
  private val LineHeight = 34

  private val lineEditors = Buffer.empty[InlineCssTextArea]

  private val blocksBox = new VBox:
    spacing = 2
    padding = Insets(0)
    style = "-fx-background-color: #f4f4f5;"

  val root: ScrollPane = new ScrollPane:
    content = blocksBox
    fitToWidth = true
    style = "-fx-background-color: #f4f4f5;"

  buildBlocks(initialCode)

  def currentCode: String =
    lineEditors.map(_.getText).mkString("\n")

  def createLineEditor(text: String): InlineCssTextArea =
    new InlineCssTextArea:
      replaceText(text)
      setPrefHeight(LineHeight)
      setMinHeight(LineHeight)
      setMaxHeight(LineHeight)
      setPrefWidth(1000)
      setStyle(
        """
          |-fx-font-family: 'Consolas';
          |-fx-font-size: 15px;
          |-fx-background-color: #f4f4f5;
          |-fx-padding: 6 0 0 0;
          |""".stripMargin
      )
      textProperty().addListener { (_, _, _) =>
        SyntaxHighlighter.applyTo(this)
        onCodeChanged(currentCode)
      }
      addEventFilter(
        KeyEvent.KEY_PRESSED,
        (event: KeyEvent) =>
          event.getCode match
            case KeyCode.ENTER =>
              event.consume()
              splitLine(this)
            case KeyCode.BACK_SPACE if getCaretPosition == 0 =>
              event.consume()
              mergeWithPreviousLine(this)
            case KeyCode.DELETE if getCaretPosition == getLength =>
              event.consume()
              mergeWithNextLine(this)
            case KeyCode.UP =>
              event.consume()
              moveFocus(this, -1)
            case KeyCode.DOWN =>
              event.consume()
              moveFocus(this, 1)
            case _ =>
      )
      AutoPairingSupport.install(this)
      SyntaxHighlighter.applyTo(this)

  private def applySyntaxHighlighting(editor: InlineCssTextArea): Unit =
    val text = editor.getText

  def buildBlocks(code: String): Unit =
    blocksBox.children.clear()
    lineEditors.clear()

    code.split("\n", -1).toSeq.zipWithIndex.foreach { case (line, index) =>
      val editor = createLineEditor(line)

      lineEditors += editor
      blocksBox.children.add(codeLineRow(editor, index + 1))

    // TODO: Visualize notes with pianoroll or scope
    }

  private def codeLineRow(editor: InlineCssTextArea, lineNumber: Int): HBox =
    val editorNode = jfxNode2sfx(editor)
    HBox.setHgrow(editorNode, Priority.Always)

    new HBox:
      spacing = 0
      minHeight = LineHeight
      maxHeight = LineHeight
      prefHeight = LineHeight
      style = "-fx-background-color: #f4f4f5;"
      children = Seq(
        new Label(lineNumber.toString):
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
        ,
        editorNode
      )

  private def splitLine(editor: InlineCssTextArea): Unit =
    val index = lineEditors.indexOf(editor)

    if index >= 0 then
      val caret = editor.getCaretPosition
      val text = editor.getText

      val before = text.take(caret)
      val after = text.drop(caret)

      val lines = lineEditors.map(_.getText).toBuffer
      lines.update(index, before)
      lines.insert(index + 1, after)

      buildBlocks(lines.mkString("\n"))

      val nextEditor = lineEditors(index + 1)
      nextEditor.requestFocus()
      nextEditor.moveTo(0)

      onCodeChanged(currentCode)

  private def mergeWithPreviousLine(editor: InlineCssTextArea): Unit =
    val index = lineEditors.indexOf(editor)

    if index > 0 then
      val lines = lineEditors.map(_.getText).toBuffer

      val previous = lines(index - 1)
      val current = lines(index)
      val caretPosition = previous.length

      lines.update(index - 1, previous + current)
      lines.remove(index)

      buildBlocks(lines.mkString("\n"))

      val previousEditor = lineEditors(index - 1)
      previousEditor.requestFocus()
      previousEditor.moveTo(caretPosition)

      onCodeChanged(currentCode)

  private def mergeWithNextLine(editor: InlineCssTextArea): Unit =
    val index = lineEditors.indexOf(editor)

    if index >= 0 && index < lineEditors.length - 1 then
      val lines = lineEditors.map(_.getText).toBuffer

      val current = lines(index)
      val next = lines(index + 1)
      val caretPosition = current.length

      lines.update(index, current + next)
      lines.remove(index + 1)

      buildBlocks(lines.mkString("\n"))

      val currentEditor = lineEditors(index)
      currentEditor.requestFocus()
      currentEditor.moveTo(caretPosition)

      onCodeChanged(currentCode)

  private def moveFocus(editor: InlineCssTextArea, delta: Int): Unit =
    val currentIndex = lineEditors.indexOf(editor)
    val nextIndex = currentIndex + delta

    if nextIndex >= 0 && nextIndex < lineEditors.length then
      val caretColumn = editor.getCaretPosition.min(editor.getLength)
      val nextEditor = lineEditors(nextIndex)
      val nextCaret = caretColumn.min(nextEditor.getLength)

      nextEditor.requestFocus()
      nextEditor.moveTo(nextCaret)
