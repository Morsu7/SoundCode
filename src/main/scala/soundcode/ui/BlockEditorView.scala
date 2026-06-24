package soundcode.ui

import javafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.VBox
import scalafx.geometry.Insets
import scalafx.scene.control.ScrollPane
import org.fxmisc.richtext.CodeArea
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

  private val lineEditors = Buffer.empty[CodeArea]

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

  def createLineEditor(text: String): CodeArea =
    new CodeArea:
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
        onCodeChanged(currentCode)
      }
      addEventFilter(
        KeyEvent.KEY_PRESSED,
        (event: KeyEvent) =>
          event.getCode match
            case _ =>
      )

  def buildBlocks(code: String): Unit =
    blocksBox.children.clear()
    lineEditors.clear()

    code.linesIterator.toSeq.zipWithIndex.foreach { case (line, index) =>
      val editor = createLineEditor(line)

      lineEditors += editor
      blocksBox.children.add(codeLineRow(editor, index + 1))

    // TODO: Visualize notes with pianoroll or scope
    }

  private def codeLineRow(editor: CodeArea, lineNumber: Int): HBox =
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
