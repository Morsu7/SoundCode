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
      val parser = new SoundCodeParser
      
      val (streams, errors) = parser.parseProgram(code) match {
        case Right(programAST) => {
          print(f"Parsing succeeded. AST: $programAST\n")
        
          (Interpreter.interpret(programAST), None)
        }
          
        case Left(errorMsg) => 
          (List.empty[Stream], Some(s"Parsing failed:\n$errorMsg"))
      }
      
      dispatch(Msg.CodeParsed(streams, errors))