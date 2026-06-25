package soundcode.parser.AST
import soundcode.parser.AST.{Pattern, Atom}

object Transformations {
    sealed trait TransformationBlock extends Block

    case class Gain(pattern: Pattern[Config]) extends TransformationBlock
    case class Pan(pattern: Pattern[Config]) extends TransformationBlock
    case class Room(pattern: Pattern[Config]) extends TransformationBlock
    case class Delay(pattern: Pattern[Config]) extends TransformationBlock
    case class LowPassFilter(pattern: Pattern[Config]) extends TransformationBlock
    case class HighPassFilter(pattern: Pattern[Config]) extends TransformationBlock

    case class Unknown(name: String, pattern: Pattern[Config]) extends TransformationBlock
}