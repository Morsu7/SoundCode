package soundcode.engine

import soundcode.domain.*

trait Scheduler:
  def generateBoundedTimelines(streams: List[Stream])(using res: Resolvable[Pattern]): List[Seq[ScheduledEvent]]
  def generateInfiniteTimeline(streams: List[Stream])(using res: Resolvable[Pattern]): LazyList[ScheduledEvent]

object SchedulerImpl extends Scheduler:

  def generateBoundedTimelines(streams: List[Stream])(using res: Resolvable[Pattern]): List[Seq[ScheduledEvent]] =
    streams.map { stream =>
      val totalCycles = CycleCalculator.lengthOf(stream)
      (0 until totalCycles).flatMap(n => resolveCycle(List(stream), n)).toList
    }

  def generateInfiniteTimeline(streams: List[Stream])(using res: Resolvable[Pattern]): LazyList[ScheduledEvent] =
    def loop(nCycle: Int): LazyList[ScheduledEvent] =
      val sortedCycleEvents = resolveCycle(streams, nCycle).sortBy(_.startTime.toDouble)
      LazyList.from(sortedCycleEvents) #::: loop(nCycle + 1)
    loop(0)

  private def resolveCycle(streams: List[Stream], nCycle: Int)(using res: Resolvable[Pattern]): List[ScheduledEvent] = {
    val startPhase = Phase(nCycle.toDouble)
    val endPhase = Phase(nCycle.toDouble + 1.0)

    streams.flatMap { stream =>
      val baseEvents = res.resolve(stream.base, startPhase, endPhase, nCycle)
      baseEvents.map { baseEvent =>
        val sampleTime = baseEvent.startTime.toDouble
        val activeExtensions = stream.extensions.flatMap { extPattern =>
          res.resolve(extPattern, startPhase, endPhase, nCycle)
            .filter(e => e.startTime.toDouble <= sampleTime && e.endTime.toDouble > sampleTime)
            .map(_.element)
        }
        baseEvent.copy(appliedExtensions = activeExtensions)
      }
    }
  }