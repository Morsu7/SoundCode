package soundcode.domain

opaque type Phase = Double
object Phase:
  def apply(value: Double): Phase = value
  extension (p: Phase) def toDouble: Double = p


opaque type AbsoluteTime = Long
object AbsoluteTime:
  def apply(value: Long): AbsoluteTime = value
  extension (t: AbsoluteTime)
    def toLong: Long = t
    def +(other: Long): AbsoluteTime = AbsoluteTime(t + other)
    def -(other: Long): AbsoluteTime = AbsoluteTime(t - other)
    def <(other: AbsoluteTime): Boolean = t < other
    def <=(other: AbsoluteTime): Boolean = t <= other

opaque type Note = String
object Note:
  def apply(value: String): Note = value
  extension (n: Note) def value: String = n

opaque type Sample = String
object Sample:
  def apply(value: String): Sample = value
  extension (s: Sample) def value: String = s

case class TextPosition(startIndex: Int, endIndex: Int)

sealed trait Element

enum Sound extends Element:
  case NoteInText(note: Note, position: TextPosition)
  case SampleInText(sample: Sample, position: TextPosition)

enum AggregationPattern extends Element:
  case SubPattern(pattern: Pattern)
  case AlternationPattern(pattern: Pattern)

enum Effect extends Element:
  case Gain(value: Int)
  case Room(value: Int)

type Pattern = List[Seq[Element]]

case class Stream(base: Pattern, extensions: List[Pattern])

case class ScheduledEvent(startTime: Phase, endTime: Phase, element: Element, appliedExtensions: List[Element] = Nil)