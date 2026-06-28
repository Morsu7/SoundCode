package soundcode.engine

import soundcode.domain.*

trait Scheduler:
  def generateEvents(streams: List[Stream], nCycle: Int)(using res: Resolvable[Pattern]): List[ScheduledEvent]

object SchedulerImpl extends Scheduler:

  def generateEvents(streams: List[Stream], nCycle: Int)(using res: Resolvable[Pattern]): List[ScheduledEvent] =
    streams.flatMap { stream =>
      val baseEvents = res.resolve(stream.base, Phase(0.0), Phase(1.0), nCycle)

      baseEvents.map { baseEvent =>
        val sampleTime = baseEvent.startTime.toDouble

        val activeExtensions = stream.extensions.flatMap { extPattern =>
          res.resolve(extPattern, Phase(0.0), Phase(1.0), nCycle)
            .filter(e => e.startTime.toDouble <= sampleTime && e.endTime.toDouble > sampleTime)
            .map(_.element)
        }
        baseEvent.copy(appliedExtensions = activeExtensions)
      }
    }