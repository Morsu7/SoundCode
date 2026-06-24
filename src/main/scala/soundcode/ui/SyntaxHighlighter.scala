package soundcode.ui

import org.fxmisc.richtext.InlineCssTextArea
import java.util.Collections

object SyntaxHighlighter:
  private val StringPattern = "\"([^\"\\\\]|\\\\.)*\"".r
  private val FunctionPattern = """\b[a-zA-Z_][a-zA-Z0-9_]*(?=\()""".r

  private val DefaultStyle = "-fx-fill: #111827;"
  private val StringStyle = "-fx-fill: #16a34a;"
  private val FunctionStyle = "-fx-fill: #2563eb; -fx-font-weight: bold;"

  def applyTo(editor: InlineCssTextArea): Unit =
    val text = editor.getText

    editor.setStyle(0, text.length, DefaultStyle)

    FunctionPattern.findAllMatchIn(text).foreach { m =>
      editor.setStyle(m.start, m.end, FunctionStyle)
    }

    StringPattern.findAllMatchIn(text).foreach { m =>
      editor.setStyle(m.start, m.end, StringStyle)
    }
