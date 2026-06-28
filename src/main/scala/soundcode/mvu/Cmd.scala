package soundcode.mvu

import soundcode.parser.SoundCodeParser
import fastparse._
import soundcode.interpreter.Interpreter
import soundcode.domain.*

sealed trait Cmd:
  def run(dispatch: Msg => Unit): Unit

object Cmd:
  case object NoOp extends Cmd:
    override def run(dispatch: Msg => Unit): Unit = println(
      "No command to run."
    )

  case class ParseAndInterpret(code: String) extends Cmd:
    override def run(dispatch: Msg => Unit): Unit =
      var parser = new SoundCodeParser
      val (streams, errors) = parser.parseProgram(code) match {
        case Parsed.Success(programAST, _) => 
          (Interpreter.interpret(programAST), None)
        case f: Parsed.Failure => 
          (List.empty[Stream], Some(s"Parsing failed: ${f.msg}"))
      }
      
      dispatch(Msg.CodeParsed(streams, errors))