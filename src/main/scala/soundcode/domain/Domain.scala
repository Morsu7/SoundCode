package soundcode.domain

import scala.annotation.tailrec

// ==========================================
// 1. TIPI BASE E TEMPORALI
// ==========================================

case class Fraction(n: Long, d: Long) extends Ordered[Fraction]:
  require(d != 0, "Denominatore zero")
  private val g = gcd(n.abs, d.abs)
  val num: Long = if (d < 0) -n / g else n / g
  val den: Long = d.abs / g

  override def equals(obj: Any): Boolean = obj match
    case that: Fraction => this.num == that.num && this.den == that.den
    case _ => false

  override def hashCode(): Int = (num, den).##

  @tailrec
  private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  def +(that: Fraction): Fraction = Fraction(num * that.den + that.num * den, den * that.den)
  def -(that: Fraction): Fraction = Fraction(num * that.den - that.num * den, den * that.den)
  def *(that: Fraction): Fraction = Fraction(num * that.num, den * that.den)
  def /(that: Fraction): Fraction = Fraction(num * that.den, den * that.num)
  def toDouble: Double = num.toDouble / den.toDouble

  def compare(that: Fraction): Int = (this.num * that.den).compare(that.num * this.den)

object Fraction:
  def apply(n: Long): Fraction = Fraction(n, 1)
  def apply(n: Double): Fraction = Fraction(n.toLong, 1)
  def apply(n: Int): Fraction = Fraction(n.toLong, 1)


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

case class ScheduledEvent(startTime: Fraction, endTime: Fraction, element: Element, appliedExtensions: List[Element] = Nil)

case class Tempo(cps: Double) {
  val cycleDurationMs: Double = 1000.0 / cps

  def durationMs(start: Fraction, end: Fraction): Long =
    Math.round((end.toDouble - start.toDouble) * cycleDurationMs)

  def offsetMs(phase: Fraction): Long =
    (phase.toDouble * cycleDurationMs).toLong
}