package soundcode.parser

import org.scalatest.funsuite.AnyFunSuite
import soundcode.parser.AST._
import fastparse._

class ProgramParserSuite extends AnyFunSuite {

    def parse(input: String) = {
        val parser = new SoundCodeParser()
        parser.parseProgram(input)
    }

    def debugParse(input: String) = {
        val parser = new SoundCodeParser()
        val result = parser.parseProgram(input)

        println(s"INPUT: $input")
        println(s"RESULT: $result")

        result match {
        case Parsed.Success(ast, _) =>
            println(s"AST: $ast")
        case Parsed.Failure(_, index, _) =>
            println(s"FAILED at $index")
        }

        println("--------------")
        result
    }

    test("parse note without attachment") {
        parse("note(C)") match {
            case Parsed.Success(ast, _) =>
            assert(ast.blocks.length == 1)

            val note = ast.blocks.head.asInstanceOf[Note]
            assert(note.value == Ident("C"))
            assert(note.attachment.isEmpty)

            case f: Parsed.Failure =>
            fail(s"failed: $f")
        }
    }

    test("parse sound block") {
        parse("sound(C)") match {
            case Parsed.Success(ast, _) =>
            assert(ast.blocks.length == 1)

            val sound = ast.blocks.head.asInstanceOf[Sound]
            assert(sound.value == Ident("C"))

            case f: Parsed.Failure =>
            fail(s"failed: $f")
        }
    }

    test("parse note with sound attachment") {
        parse("note(C).sound(D)") match {
            case Parsed.Success(ast, _) =>
            assert(ast.blocks.length == 1)

            val note = ast.blocks.head.asInstanceOf[Note]

            assert(note.value == Ident("C"))
            assert(note.attachment.nonEmpty)

            val attachment = note.attachment.get
            assert(attachment.value == Ident("D"))

            case f: Parsed.Failure =>
            fail(s"failed: $f")
        }
    }
}