package soundcode.utils.parser

import fastparse.Parsed

def formatParseError(input: String, failure: Parsed.Failure): String = {
  val trace = failure.extra.trace()
  val errorIndex = trace.index
  
  // Calculate human-readable Line and Column numbers
  val linesUpToError = input.take(errorIndex).split("\n", -1)
  val lineNum = linesUpToError.length
  val colNum = linesUpToError.last.length + 1

  // Grab the exact line text where it happened
  val totalLines = input.split("\n", -1)
  val errorLineStr = if (lineNum <= totalLines.length) totalLines(lineNum - 1) else ""
  
  // Build a neat text arrow pointing at the exact character
  val caret = " " * (colNum - 1) + "^"
  
  val expectedItems = trace.terminalAggregateString

  s"""|Syntax Error at Line $lineNum, Column $colNum
      |Expected: $expectedItems
      |
      |  $errorLineStr
      |  $caret
      |""".stripMargin
}