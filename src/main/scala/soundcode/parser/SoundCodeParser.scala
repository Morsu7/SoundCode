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
    P( streamBlock.rep(1, sep = P("\n".rep())) ~ End ).map(streams => ProgramAST(streams.toList)) // One or more audio Streams

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
    P( "note" ~ "(" ~ ws ~ wrappedPattern(noteAtom) ~ ws ~ ")" ).map( NoteBlock.apply )

  private def soundBlock(using P[?]): P[SoundBlock] = 
    P( "sound" ~ "(" ~ ws ~ wrappedPattern(sampleAtom) ~ ws ~ ")" ).map(SoundBlock.apply)

  private def wrappedPattern[T <: Atom](atom: => P[T])(using P[?]): P[Pattern[T]] =
    P( "\"" ~ ws ~ pattern(atom) ~ ws ~ "\"" ) // Tollera "   hh   "

  private def pattern[T <: Atom](atom: => P[T])(using P[?]): P[Pattern[T]] =
    P( sequence(atom).rep(1, sep = P( ws ~ "," ~ ws )) ).map(seq => Pattern(seq.toList))

  // A sequence is a series of elements played in sequence, each element lasts for a fraction of the cycle
  private def sequence[T <: Atom](atom: => P[T])(using P[?]): P[Sequence[T]] =
    P( element(atom).rep(1, sep = P( ws )) ).map(seq => Sequence(seq.toList))
  
  private def element[T <: Atom](atom: => P[T])(using P[?]): P[Element[T]] = P(
    baseElement(atom) ~ (StringIn("*", "/").! ~ configAtom).?
  ).map {
    case (base, Some((opStr, factor))) => 
      SpeedModifiedElement(base, opStr == "*", factor)
    case (base, None) => 
      base // Restituisce l'elemento base puro
  }

  // An element is either an atom (note/sample) or a sub-pattern
  private def baseElement[T <: Atom](atom: => P[T])(using P[?]): P[Element[T]] =
    P( 
      atom.map(a => AtomElement[T](a): Element[T]) | 
      subPattern(atom).map(s => s: Element[T]) | 
      alternationPattern(atom).map(a => a: Element[T])
    )
  
  private def subPattern[T <: Atom](atom: => P[T])(using P[?]): P[SubPatternElement[T]] =
    P( "[" ~ ws ~ pattern(atom) ~ ws ~ "]" ).map(SubPatternElement.apply)

  private def alternationPattern[T <: Atom](atom: => P[T])(using P[?]): P[AlternationElement[T]] =
    P( "<" ~ ws ~ pattern(atom) ~ ws ~ ">" ).map(AlternationElement.apply)

  /*
    ---------- TRANS PARSER ----------
  */

  // clean rules for transformations permitted in juxtaposition and offset, without the unknown extension
  private def inlineTransformation(using P[?]): P[TransformationBlock] = P(
    gain | pan | room | delay | lowPassFilter | highPassFilter | 
    fastForward | slowMotion | early | late | reverse | repetition
  )

  private def transformationBlock(using P[?]): P[TransformationBlock] =
  P(
    inlineTransformation 
      | offset 
      | juxtaposition
      | unknownExtension
  )

  private def unknownExtension(using P[?]): P[Unknown] = P(
    identifier ~ "(" ~ wrappedPattern(configAtom) ~ ")"
  ).map { case (name, pat) => Unknown(name, pat) }
  
  private def gain(using P[?]): P[Gain] =

    P( "gain" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(Gain.apply)
  private def pan(using P[?]): P[Pan] =
    P( "pan" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(Pan.apply)
  
  private def room(using P[?]): P[Room] =
    P( "room" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(Room.apply)

  private def delay(using P[?]): P[Delay] =
    P( "delay" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(Delay.apply)

  private def lowPassFilter(using P[?]): P[LowPassFilter] =
    P( "lpf" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(LowPassFilter.apply)

  private def highPassFilter(using P[?]): P[HighPassFilter] =
    P( "hpf" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(HighPassFilter.apply)

  private def fastForward(using P[?]): P[FastForward] =
    P( "fast" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(FastForward.apply)

  private def slowMotion(using P[?]): P[SlowMotion] =
    P( "slow" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(SlowMotion.apply)

  private def early(using P[?]): P[Early] =
    P( "early" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(Early.apply)

  private def late(using P[?]): P[Late] =
    P( "late" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(Late.apply)

  private def reverse(using P[?]): P[Reverse] =
    P( "rev()" ).map(_ => Reverse())

  private def repetition(using P[?]): P[Repetition] =
    P( "ply" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ ")" ).map(Repetition.apply)

  private def juxtaposition(using P[?]): P[Juxtaposition] = 
    P( 
      "jux" ~ "(" ~ ws ~ 
      inlineTransformation.rep(1, sep = P(ws ~ "," ~ ws)) ~ 
      ws ~ ")" 
    ).map(transSeq => Juxtaposition(transSeq.toList))

  private def offset(using P[?]): P[Offset] = 
    P( 
      "off" ~ "(" ~ ws ~ wrappedPattern(configAtom) ~ ws ~ "," ~ ws ~ 
      inlineTransformation.rep(1, sep = CharsWhileIn(" \t", 1)) ~ 
      ws ~ ")" 
    ).map { case (offsetPattern, transSeq) => 
      Offset(offsetPattern, transSeq.toList) 
    }
  /*
    ---------- ATOMS PARSER ----------
  */
  // A note is a pitch (a-g) followed by an optional octave (0-9)
  private def noteAtom(using P[?]): P[Note] = P(
    // note name (case-insensitive, lowercase in AST)
    CharIn("a-gA-G").! ~ 
    StringIn("#", "s", "b").!.? ~ 
    CharsWhileIn("0-9").!.?
  ).map { case (pitch, accidentalOpt, octaveOpt) =>
    val cleanAccidental = accidentalOpt.map(a => if (a == "s") "#" else a)
    val octave = octaveOpt.map(_.toInt).getOrElse(4)
    Note(pitch.toLowerCase, cleanAccidental, octave)
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

  // parser for whitespace (spaces and tabs)
  private def ws(using P[?]): P[Unit] = P( CharsWhileIn(" \t", 0) )
}