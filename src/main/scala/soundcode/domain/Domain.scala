package soundcode.domain

case class Stream(base: Pattern, extensions: List[Pattern])

type Pattern= List[Seq[Element]]

enum Element:
  case Sound
  case AggregationPattern
  case Effect

enum Sound:
  case NoteInText
  case SampleInText

case class NoteInText(nota:Note,position: TextPosition)
case class SampleInText(sample:Sample,position: TextPosition)
case class TextPosition(startIndex: Int, endIndex:Int)

enum AggregationPattern:
  case SubPattern(pattern: Pattern)
  case AlternationPattern(pattern: Pattern)

enum Effect:
  case Gain(value: Int)
  case Room(value: Int)
 
type Note = String
type Sample = String