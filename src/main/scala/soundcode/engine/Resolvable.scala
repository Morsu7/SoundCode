package soundcode.engine

import soundcode.domain.*

trait Resolvable[T]:
  def resolve(item: T, start: Phase, end: Phase, nCycle: Int): List[ScheduledEvent]

object Resolvable:

  given patternResolvable(using elemRes: Resolvable[Element]): Resolvable[Pattern] with
    def resolve(pattern: Pattern, start: Phase, end: Phase, nCycle: Int): List[ScheduledEvent] =
      if pattern.isEmpty then return Nil
      val s = start.toDouble
      val e = end.toDouble

      pattern.flatMap { layer =>
        if layer.isEmpty then Nil
        else
          val stepDuration = (e - s) / layer.size
          layer.zipWithIndex.flatMap { (element, stepIndex) =>
            val stepStart = Phase(s + (stepIndex * stepDuration))
            val stepEnd = Phase(stepStart.toDouble + stepDuration)
            elemRes.resolve(element, stepStart, stepEnd, nCycle)
          }
      }

  given elementResolvable: Resolvable[Element] with
    def resolve(element: Element, start: Phase, end: Phase, nCycle: Int): List[ScheduledEvent] =
      element match
        case s: Sound => List(ScheduledEvent(start, end, s))
        case e: AudioEffect => List(ScheduledEvent(start, end, e))
        case RecursivePattern.SubPattern(sub) =>
          summon[Resolvable[Pattern]].resolve(sub, start, end, nCycle)
        case RecursivePattern.AlternationPattern(alt) =>
          alt.flatMap { layer =>
            if layer.isEmpty then Nil
            else
              val choice = layer(nCycle % layer.size)
              val tempPattern = List(Seq(choice))
              //summon[Resolvable[Pattern]].resolve(tempPattern, start, end, nCycle)
              summon[Resolvable[Pattern]].resolve(tempPattern, start, end, nCycle / layer.size)
          }
        case t: RecursivePattern.Transform =>
          summon[Resolvable[RecursivePattern.Transform]].resolve(t, start, end, nCycle)

  given transformResolvable(using patRes: Resolvable[Pattern]): Resolvable[RecursivePattern.Transform] with
    def resolve(node: RecursivePattern.Transform, start: Phase, end: Phase, nCycle: Int): List[ScheduledEvent] =
      val baseEvents = patRes.resolve(node.pattern, start, end, nCycle)

      node.modifier match
        case PatternModifier.Reverse => ???

        case PatternModifier.FastForward(factor) => ???

        case PatternModifier.SlowMotion(factor) => ???

        case PatternModifier.Early(offset) => ???

        case PatternModifier.Late(offset) => ???

        case PatternModifier.Delay(_) => ???

        case PatternModifier.Repetition(_) => ???
