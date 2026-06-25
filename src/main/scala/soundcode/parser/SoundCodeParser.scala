package soundcode.parser

import fastparse._, NoWhitespace._
import soundcode.parser.AST._
import soundcode.parser.AST.Transformations._

class SoundCodeParser {

  /**
    Entrypoint for parsing a SoundCode program
    It takes a string input and returns a Parsed[ProgramAST] result, which can be either a success or a failure.
  **/
  def parseProgram(input: String): Parsed[ProgramAST] = {
    parse(input, prog(using _))
  }

  private def prog(using P[?]): P[ProgramAST] = 
    P(streamBlock.rep(1, sep = P("\n"))).map(streams => ProgramAST(streams.toList)) // One or more audio Streams

  // A "stream" is a generative block optionally chained with generative or transformation blocks
  private def streamBlock(using P[?]): P[StreamBlock] =
    P( generativeBlock ~ ( "." ~ extensionBlock ).rep ).map {
      case (baseBlock, extensionSeq) => StreamBlock(baseBlock, extensionSeq.toList)
    }

  private def generativeBlock(using P[?]): P[GenerativeBlock] =
    P( soundBlock | noteBlock )

  private def extensionBlock(using P[?]): P[ExtensionBlock] =
    P( generativeBlock.map(GenerativeExtensionBlock.apply) | transformationBlock.map(TransformationExtensionBlock.apply) )

  private def noteBlock(using P[?]): P[NoteBlock] =
    P( "note" ~ "(" ~ pattern(noteAtom) ~ ")" ).map( NoteBlock.apply )

  private def soundBlock(using P[?]): P[SoundBlock] =
    P( "sound" ~ "(" ~ pattern(sampleAtom) ~ ")" ).map( SoundBlock.apply )

  // A pattern is a series of sequences played in parallel, each sequence lasts exactly for a cycle
  private def pattern[T <: Atom](atom: => P[T])(using P[?]): P[Pattern[T]] =
    P( sequence(atom).rep(1, sep = P( CharsWhileIn(" \t").? ~ "," ~ CharsWhileIn(" \t").? )) ).map(seq => Pattern(seq.toList))

  // A sequence is a series of elements played in sequence, each element lasts for a fraction of the cycle
  private def sequence[T <: Atom](atom: => P[T])(using P[?]): P[Sequence[T]] =
    P( element(atom).rep(1, sep = P( CharsWhileIn(" \t") )) ).map(seq => Sequence(seq.toList))
    
  // An element is either an atom (note/sample) or a sub-pattern
  private def element[T <: Atom](atom: => P[T])(using P[?]): P[Element[T]] =
    P( 
      atom.map(a => AtomElement[T](a): Element[T]) | 
      subPattern(atom).map(s => s: Element[T]) | 
      alternationPattern(atom).map(a => a: Element[T])
    )

  // A sub-pattern is a pattern enclosed in square brackets ([...])
  private def subPattern[T <: Atom](atom: => P[T])(using P[?]): P[SubPatternElement[T]] =
    P( "[" ~ pattern(atom) ~ "]" ).map(SubPatternElement.apply)

  private def alternationPattern[T <: Atom](atom: => P[T])(using P[?]): P[AlternationElement[T]] =
    P( "<" ~ pattern(atom) ~ ">" ).map(AlternationElement.apply)

  /*
    ---------- TRANS PARSER ----------
  */
  private def transformationBlock(using P[?]): P[TransformationBlock] =
  P(
    gain 
      | pan 
      | room 
      | delay 
      | lowPassFilter 
      | highPassFilter 
      | fastForward
      | slowMotion
      | early
      | late
      | reverse
      | repetition
      | unknownExtension
  )

  private def unknownExtension(using P[?]): P[Unknown] = P(
    identifier ~ "(" ~ pattern(configAtom) ~ ")"
  ).map { case (name, pat) => Unknown(name, pat) }
  
  private def gain(using P[?]): P[Gain] =
    P( "gain" ~ "(" ~ pattern(configAtom) ~ ")" ).map(Gain.apply)

  private def pan(using P[?]): P[Pan] =
    P( "pan" ~ "(" ~ pattern(configAtom) ~ ")" ).map(Pan.apply)
  
  private def room(using P[?]): P[Room] =
    P( "room" ~ "(" ~ pattern(configAtom) ~ ")" ).map(Room.apply)

  private def delay(using P[?]): P[Delay] =
    P( "delay" ~ "(" ~ pattern(configAtom) ~ ")" ).map(Delay.apply)

  private def lowPassFilter(using P[?]): P[LowPassFilter] =
    P( "lpf" ~ "(" ~ pattern(configAtom) ~ ")" ).map(LowPassFilter.apply)

  private def highPassFilter(using P[?]): P[HighPassFilter] =
    P( "hpf" ~ "(" ~ pattern(configAtom) ~ ")" ).map(HighPassFilter.apply)

  private def fastForward(using P[?]): P[FastForward] =
    P( "fast" ~ "(" ~ pattern(configAtom) ~ ")" ).map(FastForward.apply)

  private def slowMotion(using P[?]): P[SlowMotion] =
    P( "slow" ~ "(" ~ pattern(configAtom) ~ ")" ).map(SlowMotion.apply)

  private def early(using P[?]): P[Early] =
    P( "early" ~ "(" ~ pattern(configAtom) ~ ")" ).map(Early.apply)

  private def late(using P[?]): P[Late] =
    P( "late" ~ "(" ~ pattern(configAtom) ~ ")" ).map(Late.apply)

  private def reverse(using P[?]): P[Reverse] =
    P( "rev()" ).map(_ => Reverse())

  private def repetition(using P[?]): P[Repetition] =
    P( "ply" ~ "(" ~ pattern(configAtom) ~ ")" ).map(Repetition.apply)

  /*
    ---------- ATOMS PARSER ----------
  */
  // A note is a pitch (a-g) followed by an optional octave (0-9)
  private def noteAtom(using P[?]): P[Note] =
    P( CharIn("a-gA-G").! ~ CharIn("0-9").?.! ).map { 
      case (pitch, octave) => Note(pitch + octave) 
    }
  // A sample is a string of lowercase letters and numbers
  private def sampleAtom(using P[?]): P[Sample] =
    P( CharsWhileIn("a-z0-9").! ).map(Sample.apply)

  private def configAtom(using P[?]): P[Config] =
    P( (CharIn("+\\-").? ~ CharsWhileIn("0-9") ~ ("." ~ CharsWhileIn("0-9")).?).! ).map(c => Config(c.toDouble))

  
  /*
    ---------- UTILS PARSER ----------
  */
  // parser for generic names
  private def identifier(using P[?]): P[String] = P( CharsWhileIn("a-zA-Z").! )
}