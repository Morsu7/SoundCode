package soundcode.ui

import org.fxmisc.richtext.InlineCssTextArea

import javafx.scene.control.Label
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.input.{KeyEvent, KeyCode}
import soundcode.ui.editor.BlockEditorView
import org.scalatest.BeforeAndAfterAll

class BlockEditorViewSpec extends UITestSupport with BeforeAndAfterAll:
  private val initialCode =
    "note(\"c4 e4 g4\").sound(\"piano\").slow(2)\n" +
      "sound(\"bd sn hh\").bank(\"tr909\")\n" +
      "note(\"a3 c4 e4\").sound(\"saw\").gain(0.7)\n" +
      "stack(note(\"c5\").sound(\"pluck\"), sound(\"hh*8\"))"

  private def editorWithInitialCode(): BlockEditorView =
    val editor = new BlockEditorView
    editor.renderCode(initialCode)
    editor

  private def blocksBoxOf(editor: BlockEditorView): VBox =
    editor.root.content.value.asInstanceOf[VBox]

  private def lineNumberAt(editor: BlockEditorView, index: Int): Label =
    rowAt(editor, index).getChildren.get(0).asInstanceOf[Label]

  private def rowAt(editor: BlockEditorView, index: Int): HBox =
    val blocksBox = editor.root.content.value.asInstanceOf[VBox]
    val block = blocksBox.getChildren.get(index).asInstanceOf[VBox]
    block.getChildren.get(0).asInstanceOf[HBox]

  private def lineEditorAt(
      editor: BlockEditorView,
      index: Int
  ): InlineCssTextArea =
    rowAt(editor, index).getChildren.get(1).asInstanceOf[InlineCssTextArea]

  test("block editor exposes the current code without changing it"):
    onFxThread:
      val editor = editorWithInitialCode()
      val actual = editor.currentCode

      assert(actual == initialCode)

  test("block editor creates one visual row for each line of code"):
    onFxThread:
      val editor = editorWithInitialCode()
      val blocksBox = blocksBoxOf(editor)

      assert(blocksBox.getChildren.size == 4)

  test("block editor shows progressive line numbers"):
    onFxThread:
      val editor = editorWithInitialCode()

      assert(lineNumberAt(editor, 0).getText == "1")
      assert(lineNumberAt(editor, 3).getText == "4")

  test("line rows use the configured visual height"):
    onFxThread:
      val editor = editorWithInitialCode()
      val row = rowAt(editor, 0)

      assert(row.getPrefHeight == 34)
      assert(row.getMinHeight == 34)
      assert(row.getMaxHeight == 34)

  test("line number column uses the same width as the line height"):
    onFxThread:
      val editor = editorWithInitialCode()
      val lineNumber = lineNumberAt(editor, 0)

      assert(lineNumber.getPrefWidth == 34)
      assert(lineNumber.getMinWidth == 34)
      assert(lineNumber.getMaxWidth == 34)

  test(
    "arrow keys move focus between line editors preserving caret column when possible"
  ):
    onFxThread:
      val editor = editorWithInitialCode()

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
      val editor = editorWithInitialCode()

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
