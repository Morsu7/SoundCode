package soundcode.ui

import org.fxmisc.richtext.InlineCssTextArea
import soundcode.ui.editor.AutoPairingSupport

class AutoPairingSupportSpec extends UITestSupport:
  test("auto pairing inserts parentheses, brackets, braces and quotes"):
    onFxThread:
      val editor = new InlineCssTextArea
      AutoPairingSupport.install(editor)

      fireKeyTyped(editor, "(")
      assert(editor.getText == "()")
      assert(editor.getCaretPosition == 1)

      editor.moveTo(editor.getLength)
      fireKeyTyped(editor, "[")
      assert(editor.getText == "()[]")
      assert(editor.getCaretPosition == 3)

      editor.moveTo(editor.getLength)
      fireKeyTyped(editor, "<")
      assert(editor.getText == "()[]<>")
      assert(editor.getCaretPosition == 5)

      editor.moveTo(editor.getLength)
      fireKeyTyped(editor, "\"")
      assert(editor.getText == "()[]<>\"\"")
      assert(editor.getCaretPosition == 7)
