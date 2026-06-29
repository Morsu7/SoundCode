package soundcode.domain

// ==========================================
// 1. TIPI BASE E TEMPORALI
// ==========================================

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


// ==========================================
// 2. Syntax Element
// ==========================================

case class TextPosition(startIndex: Int, endIndex: Int)

sealed trait Element

enum Sound extends Element:
  case NoteInText(note: Note, position: TextPosition)
  case SampleInText(sample: Sample, position: TextPosition)
  case Rest(position: TextPosition)

enum RecursivePattern extends Element:
  case SubPattern(pattern: Pattern)
  case AlternationPattern(pattern: Pattern)
  case Transform(modifier: PatternModifier, pattern: Pattern)

enum AudioEffect extends Element:
  case Gain(value: Double)
  case Pan(value: Double)
  case Room(value: Double)
  case LowPass(value: Double)
  case HighPass(value: Double)


enum PatternModifier:
  case Delay(value: Double)
  case Reverse
  case Repetition(value: Double)
  case FastForward(value: Double)
  case SlowMotion(value: Double)
  case Early(value: Double)
  case Late(value: Double)
  //case Juxtaposition(transformations: List[AudioEffect])
  //case Offset(offset: Double, transformations: List[AudioEffect])


// ==========================================
// 3. COMPOSIZIONE MUSICALE
// ==========================================

// Un Pattern è una lista di sequenze suonate in parallelo.
type Pattern = List[Seq[Element]]

case class Track(base: Pattern, extensions: List[Pattern])


// ==========================================
// 4. ESECUZIONE E SCHEDULING
// ==========================================

case class ScheduledEvent(startTime: Phase, endTime: Phase, element: Element, appliedExtensions: List[Element] = Nil)

case class Tempo(cps: Double) {
  val cycleDurationMs: Double = 1000.0 / cps

  def durationMs(start: Phase, end: Phase): Long =
    Math.round((end.toDouble - start.toDouble) * cycleDurationMs)

  def offsetMs(phase: Phase): Long =
    (phase.toDouble * cycleDurationMs).toLong
}