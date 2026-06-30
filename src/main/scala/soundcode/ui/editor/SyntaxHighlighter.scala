package soundcode.ui.editor

import org.fxmisc.richtext.GenericStyledArea
import soundcode.ui.UITheme

object SyntaxHighlighter:
  private val StringPattern = "\"([^\"\\\\]|\\\\.)*\"".r
  private val FunctionPattern = """\b[a-zA-Z_][a-zA-Z0-9_]*(?=\()""".r

  private val DefaultStyle =
    UITheme.textStyle(UITheme.Foreground)

  private val StringStyle =
    UITheme.textStyle(UITheme.String)

  private val FunctionStyle =
    s"${UITheme.textStyle(UITheme.Function)} -fx-font-weight: bold;"

  def applyTo(area: GenericStyledArea[?, ?, String]): Unit =
    val text = area.getText

    area.setStyle(0, text.length, DefaultStyle)

    FunctionPattern.findAllMatchIn(text).foreach { m =>
      area.setStyle(m.start, m.end, FunctionStyle)
    }

    StringPattern.findAllMatchIn(text).foreach { m =>
      area.setStyle(m.start, m.end, StringStyle)
    }
