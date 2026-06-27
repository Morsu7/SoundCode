package soundcode.engine

import soundcode.domain.*

trait Scheduler {
  def updateTimeline(streams: List[Stream]): Unit
  def generateEvents(nCycle: Int): List[ScheduledEvent]
}

object SchedulerImpl extends Scheduler {

  @volatile private var activeStreams: List[Stream] = Nil

  override def updateTimeline(streams: List[Stream]): Unit = activeStreams = streams

  override def generateEvents(nCycle: Int): List[ScheduledEvent] = {
    activeStreams.flatMap { stream =>
      // Il calcolo avviene SEMPRE nello spazio relativo del ciclo 0.0 -> 1.0
      val baseEvents = resolvePattern(stream.base, 0.0, 1.0, nCycle)

      baseEvents.map { baseEvent =>
        val sampleTime = baseEvent.startTime

        val activeExtensions = stream.extensions.flatMap { extPattern =>
          val extEvents = resolvePattern(extPattern, 0.0, 1.0, nCycle)
          extEvents.filter(e => e.startTime <= sampleTime && e.endTime > sampleTime)
            .map(_.element)
        }
        baseEvent.copy(appliedExtensions = activeExtensions)
      }
    }
  }

  private def resolvePattern(pattern: Pattern, start: Double, end: Double, nCycle: Int): List[ScheduledEvent] = {
    if (pattern.isEmpty) return Nil

    pattern.flatMap { layer =>
      if (layer.isEmpty) Nil
      else {
        val stepDuration = (end - start) / layer.size

        layer.zipWithIndex.flatMap { case (element, stepIndex) =>
          val stepStart = start + (stepIndex * stepDuration)
          val stepEnd = stepStart + stepDuration

          element match {
            case sound: Sound => List(ScheduledEvent(stepStart, stepEnd, sound))
            case effect: Effect => List(ScheduledEvent(stepStart, stepEnd, effect))
            case AggregationPattern.SubPattern(sub) => resolvePattern(sub, stepStart, stepEnd, nCycle)
            case AggregationPattern.AlternationPattern(alt) =>
              alt.flatMap { layer =>
                if (layer.isEmpty) Nil
                else {
                  val choice = layer(nCycle % layer.size)
                  resolvePattern(List(Seq(choice)), stepStart, stepEnd, nCycle)
                }
              }
          }
        }
      }
    }
  }
}