package soundcode.domain

type Samples = SampleInText | AggregationPattern[SampleInText]
type Notes   = NoteInText   | AggregationPattern[NoteInText]

case class Stream(
  samplePattern: Option[Pattern[Samples]] = None,
  notePattern: Option[Pattern[Notes]] = None,
  effectPatterns: List[Pattern[Effect]] = List()
)

type Pattern[+E <: Element] = List[Seq[E]]

sealed trait Element

sealed trait Sound extends Element
case class NoteInText(note: Note, position: TextPosition) extends Sound
case class SampleInText(sample: Sample, position: TextPosition) extends Sound

enum AggregationPattern[+E <: Element] extends Element:
  case SubPattern(pattern: Pattern[E]) extends AggregationPattern[E]
  case AlternationPattern(pattern: Pattern[E]) extends AggregationPattern[E]

enum Effect extends Element:
  case Gain(value: Double)
  case Pan(value: Double)
  case Room(value: Double)
  case Delay(value: Double)
  case LowPass(value: Double)
  case HighPass(value: Double)
  case Reverse
  case Repetition(value: Double)
  case FastForward(value: Double)
  case SlowMotion(value: Double)
  case Early(value: Double)
  case Late(value: Double)
  case Juxtaposition(transformations: List[Effect])
  case Offset(offset: Double, transformations: List[Effect])

case class TextPosition(startIndex: Int, endIndex: Int)

type Note = String
type Sample = String