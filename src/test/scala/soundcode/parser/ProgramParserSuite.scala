package soundcode.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import soundcode.parser.AST._
import soundcode.parser.AST.Transformations._

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
            val stream = ast.blocks.head.asInstanceOf[StreamBlock]
            val block = stream.base.asInstanceOf[SoundBlock]
            
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
            val stream = ast.blocks.head.asInstanceOf[StreamBlock]
            val block = stream.base.asInstanceOf[NoteBlock]

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
            val stream = ast.blocks.head.asInstanceOf[StreamBlock]
            val block = stream.base.asInstanceOf[SoundBlock]

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
            val stream = ast.blocks.head.asInstanceOf[StreamBlock]
            val block = stream.base.asInstanceOf[SoundBlock]

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
            val stream = ast.blocks.head.asInstanceOf[StreamBlock]
            val block = stream.base.asInstanceOf[SoundBlock]
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
            val stream = ast.blocks.head.asInstanceOf[StreamBlock]
            val noteBlock = stream.base.asInstanceOf[NoteBlock]

            assert(stream.extensions.size == 1)
            val attachedSound = stream.extensions.head.asInstanceOf[GenerativeExtensionBlock].block.asInstanceOf[SoundBlock]
            
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
            assert(ast.blocks(0).asInstanceOf[StreamBlock].base.isInstanceOf[SoundBlock])
            assert(ast.blocks(1).asInstanceOf[StreamBlock].base.isInstanceOf[NoteBlock])
            assert(ast.blocks(2).asInstanceOf[StreamBlock].base.isInstanceOf[SoundBlock])

        case f: Parsed.Failure =>
            fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("complex mixed pattern") {
        parse("sound(bd hh, [bd <hh oh>])") match {
        case Parsed.Success(ast, _) =>
            val stream = ast.blocks.head.asInstanceOf[StreamBlock]
            val block = stream.base.asInstanceOf[SoundBlock]

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

    test("single stream with no extensions") {
        val input = "sound(bd)"
        val result = parse(input)
        
        assert(result.isInstanceOf[fastparse.Parsed.Success[?]])
        val program = result.get.value
        
        val stream = program.blocks.head.asInstanceOf[StreamBlock]
        assert(stream.base.isInstanceOf[SoundBlock])
        assert(stream.extensions.isEmpty == true)
    }

    test("mixed chain: generative + transformation") {
        val input = "note(c4).sound(bd).gain(0.8)"
        val result = parse(input)
        
        assert(result.isInstanceOf[fastparse.Parsed.Success[?]])
        val program = result.get.value
        val stream = program.blocks.head.asInstanceOf[StreamBlock]
        
        assert(stream.base.isInstanceOf[NoteBlock])
        
        assert(stream.extensions.size == 2)
        
        val ext1 = stream.extensions(0).asInstanceOf[GenerativeExtensionBlock]
        assert(ext1.block.isInstanceOf[SoundBlock])
        
        val ext2 = stream.extensions(1).asInstanceOf[TransformationExtensionBlock]
        assert(ext2.block.isInstanceOf[Gain])
    }

    test("multiple chains on multiple lines") {
        val input = "sound(hh).fast(2)\nnote(e3).pan(0.1)"
            
        val result = parse(input)
        assert(result.isInstanceOf[fastparse.Parsed.Success[?]])
        val program = result.get.value
        
        assert(program.blocks.size == 2)
    }

    test("parsing failure for invalid input") {
        val input = ".gain(0.8).sound(bd)"
        val result = parse(input)
        
        assert(result.isInstanceOf[fastparse.Parsed.Failure])
    }

    test("unknown transformation block") {
        val input = "sound(bd).customTransform(1.5)"
        val result = parse(input)
        
        assert(result.isInstanceOf[fastparse.Parsed.Success[?]])
        val program = result.get.value
        val stream = program.blocks.head.asInstanceOf[StreamBlock]
        
        assert(stream.base.isInstanceOf[SoundBlock])
        assert(stream.extensions.size == 1)
        
        val ext = stream.extensions.head.asInstanceOf[TransformationExtensionBlock]
        assert(ext.block.isInstanceOf[Unknown])
        
        val unknownBlock = ext.block.asInstanceOf[Unknown]
        assert(unknownBlock.name == "customTransform")
        assert(unknownBlock.pattern.elems.head.elems.head.asInstanceOf[AtomElement[Config]].atom.value == 1.5)
    }
}