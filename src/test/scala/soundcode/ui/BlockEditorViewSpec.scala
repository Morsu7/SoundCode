package soundcode.ui

import soundcode.ui.editor.BlockEditorView

class BlockEditorViewSpec extends UITestSupport:
  test("block editor can be created"):
    onFxThread:
      val editor = new BlockEditorView

      assert(editor.root != null)

  test("block editor starts with empty code"):
    onFxThread:
      val editor = new BlockEditorView

      assert(editor.currentCode == "")
