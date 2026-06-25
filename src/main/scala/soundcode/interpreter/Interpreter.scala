package soundcode.interpreter

import soundcode.parser.AST._
import soundcode.parser.AST.Transformations._
import soundcode.domain

object Interpreter {

    def interpret(tree: ProgramAST): List[domain.Stream] = {
        tree.blocks.map { case StreamBlock(base, extensions) =>
            // Extraxt generative blocks (SoundBlock and NoteBlock) from the base and extensions
            // note: only one sound and note block are allowed per stream, we will take the first and ignore the rest
            val generativeBlocks = base :: extensions.collect {
                case GenerativeExtensionBlock(genBlock) => genBlock
            }
            
            val samplesPattern: Option[domain.Pattern[domain.SampleInText]] = 
                generativeBlocks.collectFirst { case SoundBlock(p) => 
                    interpretPattern(p)(interpretSampleAtom).asInstanceOf[domain.Pattern[domain.SampleInText]]
                }
            val notesPattern: Option[domain.Pattern[domain.NoteInText]] = 
                generativeBlocks.collectFirst { case NoteBlock(p) => 
                    interpretPattern(p)(interpretNoteAtom).asInstanceOf[domain.Pattern[domain.NoteInText]]
                }

            val effectPatterns: List[domain.Pattern[domain.Effect]] = extensions.collect {
                case TransformationExtensionBlock(transBlock) => 
                    interpretTransformationBlock(transBlock)
            }

            domain.Stream(
                samplePattern = samplesPattern,
                notePattern = notesPattern,
                effectPatterns = effectPatterns
            )
        }
    }

    private def interpretTransformationBlock(block: TransformationBlock): domain.Pattern[domain.Effect] = {
        block match {
            case Reverse() => List(Seq(domain.Effect.Reverse))
            case Juxtaposition(ts) => ts.flatMap(interpretTransformationBlock)
            case Offset(_, ts)     => ts.flatMap(interpretTransformationBlock)
            case Unknown(name, _)  => throw new IllegalArgumentException(s"Unknown: $name")
            
            // Gestiamo le trasformazioni come BUILDER di intere strutture Pattern
            case effectBlock =>
            block match {
                // Se nel tuo dominio Gain vuole direttamente il Pattern[Double] o Pattern[Effect],
                // usiamo interpretPattern per convertire l'albero di Config in elementi del dominio
                case Gain(p)           => interpretPattern(p)(c => domain.Effect.Gain(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case Pan(p)            => interpretPattern(p)(c => domain.Effect.Pan(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case Room(p)           => interpretPattern(p)(c => domain.Effect.Room(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case Delay(p)          => interpretPattern(p)(c => domain.Effect.Delay(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case LowPassFilter(p)  => interpretPattern(p)(c => domain.Effect.LowPass(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case HighPassFilter(p) => interpretPattern(p)(c => domain.Effect.HighPass(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case Repetition(p)     => interpretPattern(p)(c => domain.Effect.Repetition(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case FastForward(p)    => interpretPattern(p)(c => domain.Effect.FastForward(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case SlowMotion(p)     => interpretPattern(p)(c => domain.Effect.SlowMotion(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case Early(p)          => interpretPattern(p)(c => domain.Effect.Early(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case Late(p)           => interpretPattern(p)(c => domain.Effect.Late(c.value)).asInstanceOf[domain.Pattern[domain.Effect]]
                case _                 => throw new MatchError(effectBlock)
            }
        }
    }

    private def interpretPattern[A <: Atom](pattern: Pattern[A])(buildAtom: A => domain.Element): domain.Pattern[domain.Element] = {
        pattern.elems.map { sequence => 
            sequence.elems.map { element => interpretElement(element)(buildAtom) }
        }
    }

    private def interpretElement[A <: Atom](element: Element[A])(buildAtom: A => domain.Element): domain.Element = {
        element match {
        case AtomElement(atom) => buildAtom(atom)
        case SubPatternElement(p) => domain.AggregationPattern.SubPattern(interpretPattern(p)(buildAtom))
        case AlternationElement(p) => domain.AggregationPattern.AlternationPattern(interpretPattern(p)(buildAtom))
        case SpeedModifiedElement(base, _, _) => interpretElement(base)(buildAtom)
        }
    }

    // --- 4. LE LOGICHE ATOMICHE SEPARATE ---
    private def interpretSampleAtom(atom: Atom): domain.Element = atom match {
        case Sample(value) => domain.SampleInText(value, domain.TextPosition(0, 0))
        case _ => throw new IllegalArgumentException("Expected Sample")
    }

    private def interpretNoteAtom(atom: Atom): domain.Element = atom match {
        case Note(name, accidental, octave) => 
        val noteStr = s"$name${accidental.getOrElse("")}${octave}"
        domain.NoteInText(noteStr, domain.TextPosition(0, 0))
        case _ => throw new IllegalArgumentException("Expected Note")
    }
}