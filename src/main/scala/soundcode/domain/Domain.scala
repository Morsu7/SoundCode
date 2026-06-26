package soundcode.domain

case class Stream(base: Pattern, extensions: List[Pattern])

type Pattern = List[Seq[Element]]

sealed trait Element

enum Sound extends Element:
  case NoteInText(nota: Note, position: TextPosition)
  case SampleInText(sample: Sample, position: TextPosition)

enum AggregationPattern extends Element:
  case SubPattern(pattern: Pattern)
  case AlternationPattern(pattern: Pattern)

enum Effect extends Element:
  case Gain(value: Int)
  case Room(value: Int)

case class TextPosition(startIndex: Int, endIndex: Int)

case class ScheduledEvent(startTime: Double, endTime: Double, element: Element, appliedExtensions: List[Element] = Nil)

type Note = String
type Sample = String