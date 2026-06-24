package soundcode.ui

import org.fxmisc.richtext.InlineCssTextArea

import javafx.scene.control.Label
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.input.{KeyEvent, KeyCode}

class BlockEditorViewSpec extends UITestSupport:
  private val initialCode =
    """note("c4 e4 g4").sound("piano").slow(2)
      |sound("bd sn hh").bank("tr909")
      |note("a3 c4 e4").sound("saw").gain(0.7)
      |stack(note("c5").sound("pluck"), sound("hh*8"))""".stripMargin

  private def lineEditorAt(
      editor: BlockEditorView,
      index: Int
  ): InlineCssTextArea =
    val blocksBox = editor.root.content.value.asInstanceOf[VBox]
    val row = blocksBox.getChildren.get(index).asInstanceOf[HBox]
    row.getChildren.get(1).asInstanceOf[InlineCssTextArea]

  test("block editor exposes the current code without changing it"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())

      assert(editor.currentCode == initialCode)

  test("block editor creates one visual row for each line of code"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]

      assert(blocksBox.getChildren.size == 4)

  test("block editor shows progressive line numbers"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]

      val firstRow = blocksBox.getChildren.get(0).asInstanceOf[HBox]
      val fourthRow = blocksBox.getChildren.get(3).asInstanceOf[HBox]

      val firstLabel = firstRow.getChildren.get(0).asInstanceOf[Label]
      val fourthLabel = fourthRow.getChildren.get(0).asInstanceOf[Label]

      assert(firstLabel.getText == "1")
      assert(fourthLabel.getText == "4")

  test("line rows use the configured visual height"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]
      val row = blocksBox.getChildren.get(0).asInstanceOf[HBox]

      assert(row.getPrefHeight == 34)
      assert(row.getMinHeight == 34)
      assert(row.getMaxHeight == 34)

  test("line number column uses the same width as the line height"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]
      val row = blocksBox.getChildren.get(0).asInstanceOf[HBox]
      val lineNumber = row.getChildren.get(0).asInstanceOf[Label]

      assert(lineNumber.getPrefWidth == 34)
      assert(lineNumber.getMinWidth == 34)
      assert(lineNumber.getMaxWidth == 34)

  test("changing a line editor notifies with the full current code"):
    onFxThread:
      var lastChange = ""

      val editor = BlockEditorView(initialCode, code => lastChange = code)
      val firstInlineCssTextArea = lineEditorAt(editor, 0)

      firstInlineCssTextArea.replaceText("changed")

      val expectedCode = initialCode
        .split("\n", -1)
        .updated(0, "changed")
        .mkString("\n")

      assert(lastChange == expectedCode)

  test(
    "arrow keys move focus between line editors preserving caret column when possible"
  ):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())

      val firstLine = lineEditorAt(editor, 0)
      val secondLine = lineEditorAt(editor, 1)
      val thirdLine = lineEditorAt(editor, 2)

      firstLine.moveTo(3)
      fireKeyPressed(firstLine, KeyCode.DOWN)

      assert(secondLine.getCaretPosition == 3)

      fireKeyPressed(secondLine, KeyCode.DOWN)

      assert(thirdLine.getCaretPosition == 3)

      fireKeyPressed(thirdLine, KeyCode.UP)

      assert(secondLine.getCaretPosition == 3)

  test("line editor key codes split and merge lines"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())

      val originalLines = initialCode.split("\n", -1).toSeq

      val firstLine = lineEditorAt(editor, 0)
      firstLine.moveTo(4)
      fireKeyPressed(firstLine, KeyCode.ENTER)

      val afterEnter = originalLines
        .updated(0, originalLines.head.take(4))
        .patch(1, Seq(originalLines.head.drop(4)), 0)
        .mkString("\n")

      assert(editor.currentCode == afterEnter)

      val secondLine = lineEditorAt(editor, 1)
      secondLine.moveTo(0)
      fireKeyPressed(secondLine, KeyCode.BACK_SPACE)

      assert(editor.currentCode == initialCode)

      val firstLineAfterMerge = lineEditorAt(editor, 0)
      firstLineAfterMerge.moveTo(firstLineAfterMerge.getLength)
      fireKeyPressed(firstLineAfterMerge, KeyCode.DELETE)

      val afterDelete = originalLines
        .updated(0, originalLines(0) + originalLines(1))
        .patch(1, Nil, 1)
        .mkString("\n")

      assert(editor.currentCode == afterDelete)
