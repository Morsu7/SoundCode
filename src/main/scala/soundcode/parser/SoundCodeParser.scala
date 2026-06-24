package soundcode.parser

import fastparse._, NoWhitespace._
import soundcode.parser.AST._

class SoundCodeParser {

  /**
    Entrypoint for parsing a SoundCode program
    It takes a string input and returns a Parsed[ProgramAST] result, which can be either a success or a failure.
  **/
  def parseProgram(input: String): Parsed[ProgramAST] = {
    parse(input, prog(using _))
  }

  private def prog(using P[?]): P[ProgramAST] = 
    P(block.rep(1, sep = P("\n"))).map(ProgramAST.apply) // One or more blocks

  private def block(using P[?]): P[Block] =
    P( noteBlock | soundBlock )

  private def noteBlock(using P[?]): P[NoteBlock] =
    P( "note" ~ "(" ~ pattern(noteAtom) ~ ")" ~ ("." ~ soundBlock).? ).map(
      (value, attachment) => NoteBlock(value, attachment)
    )

  private def soundBlock(using P[?]): P[SoundBlock] =
    P( "sound" ~ "(" ~ pattern(sampleAtom) ~ ")" ).map( 
      (value) => SoundBlock(value)
    )

  // A pattern is a series of sequences played in parallel, each sequence lasts exactly for a cycle
  private def pattern[T <: Atom](atom: => P[T])(using P[?]): P[Pattern[T]] =
    P( sequence(atom).rep(1, sep = P(",")) ).map(seq => Pattern(seq.toList))

  // A sequence is a series of elements played in sequence, each element lasts for a fraction of the cycle
  private def sequence[T <: Atom](atom: => P[T])(using P[?]): P[Sequence[T]] =
    P( element(atom).rep(1, sep = P(" ")) ).map(seq => Sequence(seq.toList))

  // An element is either an atom (note/sample) or a sub-pattern
  private def element[T <: Atom](atom: => P[T])(using P[?]): P[Element[T]] =
    P( atom.map(AtomElement.apply) | subPattern(atom) )

  // A sub-pattern is a pattern enclosed in square brackets ([...])
  private def subPattern[T <: Atom](atom: => P[T])(using P[?]): P[SubPatternElement[T]] =
    P( "[" ~ pattern(atom) ~ "]" ).map(SubPatternElement.apply)


  // A note is a pitch (a-g) followed by an optional octave (0-9)
  private def noteAtom(using P[?]): P[Note] =
    P( CharIn("a-gA-G").! ~ CharIn("0-9").?.! ).map { 
      case (pitch, octave) => Note(pitch + octave) 
    }
  // A sample is a string of lowercase letters and numbers
  private def sampleAtom(using P[?]): P[Sample] =
    P( CharsWhileIn("a-z0-9").! ).map(Sample.apply)
}