package soundcode.ui.editor

import org.fxmisc.richtext.GenericStyledArea

object SyntaxHighlighter:
  private val StringPattern = "\"([^\"\\\\]|\\\\.)*\"".r
  private val FunctionPattern = """\b[a-zA-Z_][a-zA-Z0-9_]*(?=\()""".r

  private val DefaultStyle =
    "-fx-fill: #f4f4f5; -fx-font-smoothing-type: gray;"

  private val StringStyle =
    "-fx-fill: #86efac; -fx-font-smoothing-type: gray;"

  private val FunctionStyle =
    "-fx-fill: #93c5fd; -fx-font-weight: bold; -fx-font-smoothing-type: gray;"

  def applyTo(area: GenericStyledArea[?, ?, String]): Unit =
    val text = area.getText

    area.setStyle(0, text.length, DefaultStyle)

    FunctionPattern.findAllMatchIn(text).foreach { m =>
      area.setStyle(m.start, m.end, FunctionStyle)
    }

    StringPattern.findAllMatchIn(text).foreach { m =>
      area.setStyle(m.start, m.end, StringStyle)
    }
