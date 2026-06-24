package soundcode.ui

import javafx.scene.input.KeyEvent

import org.fxmisc.richtext.InlineCssTextArea

object AutoPairingSupport:
  def install(editor: InlineCssTextArea): Unit =
    editor.addEventFilter(
      KeyEvent.KEY_TYPED,
      (event: KeyEvent) =>
        event.getCharacter match
          case "\"" =>
            if shouldInsertClosingQuote(editor) then
              event.consume()
              insertPair(editor, "\"", "\"")

          case "(" =>
            event.consume()
            insertPair(editor, "(", ")")

          case "[" =>
            event.consume()
            insertPair(editor, "[", "]")

          case "<" =>
            event.consume()
            insertPair(editor, "<", ">")

          case _ =>
    )

  private def insertPair(
      editor: InlineCssTextArea,
      open: String,
      close: String
  ): Unit =
    val caret = editor.getCaretPosition
    editor.insertText(caret, open + close)
    editor.moveTo(caret + open.length)

  private def shouldInsertClosingQuote(editor: InlineCssTextArea): Boolean =
    val before = editor.getText.take(editor.getCaretPosition)
    before.count(_ == '"') % 2 == 0
