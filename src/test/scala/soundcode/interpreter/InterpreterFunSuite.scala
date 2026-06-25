package soundcode.interpreter

import org.scalatest.funsuite.AnyFunSuite
import fastparse._

import soundcode.parser.SoundCodeParser
import soundcode.domain.Stream
import soundcode.interpreter.Interpreter

import soundcode.domain.{NoteInText, SampleInText, TextPosition, AggregationPattern as AP, Effect}

class InterpreterFunSuite extends AnyFunSuite {

    private def interpret(input: String): List[Stream] = {
        val ast = new SoundCodeParser().parseProgram(input)
        ast match {
            case Parsed.Success(programAST, _) => 
                Interpreter.interpret(programAST)
            case f: Parsed.Failure => 
                fail(s"Parsing failed: ${f.msg}")
        }
    }

    test("interpret a simple note block with 2 notes") {
        interpret("note(\"c# e3\")") match {
        case s: List[Stream] =>
            assert(s.length == 1)
            val stream = s.head
            assert(!stream.samplePattern.isDefined)
            val sequence = stream.notePattern.get.head
            assert(sequence.length == 2)
            assert(sequence.head == NoteInText("c#4", TextPosition(0, 0)))
            assert(sequence(1) == NoteInText("e3", TextPosition(0, 0)))
        case null => fail("Expected a List[Stream]")
        }
    }

    test("interpret a simple sound block with 2 samples") {
        interpret("sound(\"bd hh\")") match {
        case s: List[Stream] =>
            val stream = s.head
            assert(!stream.notePattern.isDefined)
            val sequence = stream.samplePattern.get.head
            assert(sequence.length == 2)
            assert(sequence.head == SampleInText("bd", TextPosition(0, 0)))
            assert(sequence(1) == SampleInText("hh", TextPosition(0, 0)))
        case null => fail("Expected a List[Stream]")
        }
    }

    test("interpret a stream with a note block and a sound block") {
        interpret("note(\"c# e3\").sound(\"bd hh\")") match {
        case s: List[Stream] =>
            val stream = s.head
            assert(stream.notePattern.isDefined)
            assert(stream.samplePattern.isDefined)
            val noteSequence = stream.notePattern.get.head
            val sampleSequence = stream.samplePattern.get.head
            assert(noteSequence.length == 2)
            assert(sampleSequence.length == 2)
            assert(noteSequence.head == NoteInText("c#4", TextPosition(0, 0)))
            assert(noteSequence(1) == NoteInText("e3", TextPosition(0, 0)))
            assert(sampleSequence.head == SampleInText("bd", TextPosition(0, 0)))
            assert(sampleSequence(1) == SampleInText("hh", TextPosition(0, 0)))
        case null => fail("Expected a List[Stream]")
        }
    }

    test("interpret multiple streams") {
        interpret("note(\"c# e3\")\nsound(\"bd hh\")") match {
        case s: List[Stream] =>
            assert(s.length == 2)
            val stream1 = s.head
            val stream2 = s(1)
            assert(stream1.notePattern.isDefined)
            assert(!stream1.samplePattern.isDefined)
            assert(!stream2.notePattern.isDefined)
            assert(stream2.samplePattern.isDefined)
        case null => fail("Expected a List[Stream]")
        }
    }

    test("interpret a stream with 2 note blocks and 3 sound blocks (only keep the first of each)") {
        interpret("note(\"c# e3\").note(\"g4\")\nsound(\"bd hh\").sound(\"sn\").sound(\"oh\")") match {
        case s: List[Stream] =>
            assert(s.length == 2)
            val stream1 = s.head
            val stream2 = s(1)
            assert(stream1.notePattern.isDefined)
            assert(!stream1.samplePattern.isDefined)
            assert(stream1.notePattern.get.head.length == 2) // Only the first note block is kept
            assert(stream2.samplePattern.isDefined)
            assert(!stream2.notePattern.isDefined)
            assert(stream2.samplePattern.get.head.length == 2) // Only the first sound block is kept
        case null => fail("Expected a List[Stream]")
        }
    }

    test("interpret a stream with multiple transformation blocks (effects)") {
        interpret("note(\"c# e3\").gain(\"5\").room(\"10\")\nsound(\"bd hh\").gain(\"3\")") match {
        case s: List[Stream] =>
            assert(s.length == 2)
            val stream1 = s.head
            val stream2 = s(1)
            assert(stream1.notePattern.isDefined)
            assert(!stream1.samplePattern.isDefined)
            assert(stream1.effectPatterns.length == 2) // Two transformation blocks
            val effectPattern1 = stream1.effectPatterns.head
            val effectPattern2 = stream1.effectPatterns(1)
            val gainEffect = effectPattern1.head.head
            val roomEffect = effectPattern2.head.head
            assert(gainEffect == Effect.Gain(5))
            assert(roomEffect == Effect.Room(10))
            assert(stream2.samplePattern.isDefined)
            assert(!stream2.notePattern.isDefined)
            assert(stream2.effectPatterns.length == 1) // One transformation block
            val effectPattern3 = stream2.effectPatterns.head
            val gainEffect2 = effectPattern3.head.head
            assert(gainEffect2 == Effect.Gain(3))
            
        case null => fail("Expected a List[Stream]")
        }
    }

    test("interpret a stream with a note and sound block with complex patterns") {
        interpret("note(\"c# [e3 g4] <a4 b4>\").sound(\"bd [hh sn] <oh>\")") match {
            case s: List[Stream] =>
                assert(s.length == 1)
                val stream = s.head
                assert(stream.notePattern.isDefined)
                assert(stream.samplePattern.isDefined)
                
                val noteSequence = stream.notePattern.get.head
                val sampleSequence = stream.samplePattern.get.head
                
                assert(noteSequence.length == 3) // c#, [e3 g4], <a4 b4>
                // Verifica che il tuo parser gestisca l'ottava di default per far uscire "c#4" o correggi in "c#"
                assert(noteSequence.head == NoteInText("c#4", TextPosition(0, 0))) 
                
                // Corretto l'impacchettamento in List(List(...)) per rispettare type Pattern
                assert(noteSequence(1) == AP.SubPattern(List(List(
                    NoteInText("e3", TextPosition(0, 0)), 
                    NoteInText("g4", TextPosition(0, 0))
                ))))
                assert(noteSequence(2) == AP.AlternationPattern(List(List(
                    NoteInText("a4", TextPosition(0, 0)), 
                    NoteInText("b4", TextPosition(0, 0))
                ))))

                assert(sampleSequence.length == 3) // bd, [hh sn], <oh>
                assert(sampleSequence.head == SampleInText("bd", TextPosition(0, 0)))
                
                assert(sampleSequence(1) == AP.SubPattern(List(List(
                    SampleInText("hh", TextPosition(0, 0)), 
                    SampleInText("sn", TextPosition(0, 0))
                ))))
                assert(sampleSequence(2) == AP.AlternationPattern(List(List(
                    SampleInText("oh", TextPosition(0, 0))
                ))))
                
            case null => fail("Expected a List[Stream]")
        }
    }

    test("interpret a note block with 3 levels annidations") {
        interpret("note(\"c# [e3 <g4 a4>] <b4>\")") match {
            case s: List[Stream] =>
                assert(s.length == 1)
                val stream = s.head
                assert(stream.notePattern.isDefined)
                val noteSequence = stream.notePattern.get.head
                assert(noteSequence.length == 3) // c#, [e3 <g4 a4>], <b4>
                assert(noteSequence.head == NoteInText("c#4", TextPosition(0, 0)))

                assert(noteSequence(1) == AP.SubPattern(List(List(
                    NoteInText("e3", TextPosition(0, 0)), 
                    AP.AlternationPattern(List(List(
                        NoteInText("g4", TextPosition(0, 0)), 
                        NoteInText("a4", TextPosition(0, 0))
                    )))
                ))))
                
                assert(noteSequence(2) == AP.AlternationPattern(List(List(
                    NoteInText("b4", TextPosition(0, 0))
                ))))
                
            case null => fail("Expected a List[Stream]")
        }
    }
}