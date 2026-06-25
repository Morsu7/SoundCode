package soundcode.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import soundcode.parser.AST._

class SoundCodeParserSuite extends AnyFunSuite {

    private def parse(input: String) = {
        val res = new SoundCodeParser().parseProgram(input)
        println(s"\nINPUT: $input")
        
        res match {
            case Parsed.Success(ast, _) => 
            println(s"OUTPUT:\n$ast\n") // Stampa solo l'albero pulito
            case f: Parsed.Failure => 
            println(s"OUTPUT: Failure -> ${f.msg}\n")
        }
        res
    }

    test("single sound block") {
        parse("sound(bd)") match {
        case Parsed.Success(ast, _) =>
            val block = ast.blocks.head.asInstanceOf[SoundBlock]
            
            val expected = Pattern(List(
            Sequence(List(
                AtomElement(Sample("bd"))
            ))
            ))
            assert(block.pattern == expected)

        case f: Parsed.Failure =>
            fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("single note block") {
        parse("note(c4)") match {
        case Parsed.Success(ast, _) =>
            val block = ast.blocks.head.asInstanceOf[NoteBlock]

            val expected = Pattern(List(
            Sequence(List(
                AtomElement(Note("c4"))
            ))
            ))
            assert(block.pattern == expected)

        case f: Parsed.Failure =>
            fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("sequence in sound block") {
        // Nota: rimosse le quadre esterne superflue se vuoi testare una sequenza pura "bd hh sd",
        // oppure mantenute se l'obiettivo è testare esplicitamente un SubPatternElement.
        parse("sound([bd hh sd])") match {
        case Parsed.Success(ast, _) =>
            val block = ast.blocks.head.asInstanceOf[SoundBlock]

            val expected = Pattern(List(
            Sequence(List(
                SubPatternElement(Pattern(List(
                Sequence(List(
                    AtomElement(Sample("bd")),
                    AtomElement(Sample("hh")),
                    AtomElement(Sample("sd"))
                ))
                )))
            ))
            ))
            assert(block.pattern == expected)

        case f: Parsed.Failure =>
            fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("alternation in sound block") {
        parse("sound(bd <hh oh>)") match {
        case Parsed.Success(ast, _) =>
            val block = ast.blocks.head.asInstanceOf[SoundBlock]

            // Struttura attesa: un ciclo con una sequenza di 2 elementi:
            // 1. L'atomo "bd"
            // 2. L'AlternationElement che contiene a sua volta "hh" e "oh"
            val expected = Pattern(List(
            Sequence(List(
                AtomElement(Sample("bd")),
                AlternationElement(Pattern(List(
                Sequence(List(
                    AtomElement(Sample("hh")),
                    AtomElement(Sample("oh"))
                ))
                )))
            ))
            ))
            assert(block.pattern == expected)

        case f: Parsed.Failure => 
            fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("parallel in sound block") {
        parse("sound(bd,hh,sd)") match {
        case Parsed.Success(ast, _) =>
            val block = ast.blocks.head.asInstanceOf[SoundBlock]
            val expected = Pattern(List(
            Sequence(List(AtomElement(Sample("bd")))),
            Sequence(List(AtomElement(Sample("hh")))),
            Sequence(List(AtomElement(Sample("sd"))))
            ))
            assert(block.pattern == expected)
        case f: Parsed.Failure => fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("note + sound attachment") {
        parse("note(c4).sound(bd)") match {
        case Parsed.Success(ast, _) =>
            val noteBlock = ast.blocks.head.asInstanceOf[NoteBlock]

            assert(noteBlock.attachment.isDefined)
            val attachedSound = noteBlock.attachment.get
            
            val expectedSoundPattern = Pattern(List(
            Sequence(List(AtomElement(Sample("bd"))))
            ))
            assert(attachedSound.pattern == expectedSoundPattern)

        case f: Parsed.Failure =>
            fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("multiple blocks") {
        parse("sound(bd)\nnote(c4)\nsound(hh)") match {
        case Parsed.Success(ast, _) =>
            assert(ast.blocks.length == 3)
            assert(ast.blocks(0).isInstanceOf[SoundBlock])
            assert(ast.blocks(1).isInstanceOf[NoteBlock])
            assert(ast.blocks(2).isInstanceOf[SoundBlock])

        case f: Parsed.Failure =>
            fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("complex mixed pattern") {
        parse("sound(bd hh, [bd <hh oh>])") match {
        case Parsed.Success(ast, _) =>
            val block = ast.blocks.head.asInstanceOf[SoundBlock]

            val expected = Pattern(List(
                Sequence(List(
                AtomElement(Sample("bd")),
                AtomElement(Sample("hh"))
                )),
                Sequence(List(
                SubPatternElement(Pattern(List(
                    Sequence(List(
                    AtomElement(Sample("bd")),
                    AlternationElement(Pattern(List(
                        Sequence(List(
                        AtomElement(Sample("hh")),
                        AtomElement(Sample("oh"))
                        ))
                    )))
                    ))
                )))
                ))
            ))
            assert(block.pattern == expected)
        case f: Parsed.Failure => fail(s"Parsing failed: ${f.msg}")
        }
    }
}