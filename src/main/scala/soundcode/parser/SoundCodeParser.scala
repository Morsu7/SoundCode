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
    P(block.rep(1)).map(ProgramAST.apply) // One or more blocks

  private def block(using P[?]): P[Block] =
    P( note | sound)

  private def note(using P[?]): P[Note] =
    P( "note" ~ "(" ~ expression ~ ")" ~ ("." ~ sound).? ).map(
      (value, attachment) => Note(value, attachment)
    )

  private def sound(using P[?]): P[Sound] =
    P( "sound" ~ "(" ~ expression ~ ")" ).map( 
      (value) => Sound(value)
    )

  private def expression(using P[?]): P[Expr] =
    P( CharIn("A-Z").! ).map(c => Ident(c.toString))

    // TODO: Aggiungi ai requirements la descrizione della sintassi di base in modo chiaro
}