package soundcode.engine

import soundcode.domain.*

trait Scheduler:
  def generateBoundedTimelines(tracks: List[Track])(using res: Resolvable[Pattern]): List[Seq[ScheduledEvent]]
  def generateInfiniteTimeline(tracks: List[Track])(using res: Resolvable[Pattern]): LazyList[ScheduledEvent]

object SchedulerImpl extends Scheduler:

  def generateBoundedTimelines(tracks: List[Track])(using res: Resolvable[Pattern]): List[Seq[ScheduledEvent]] =
    tracks.map { stream =>
      val totalCycles = CycleCalculator.lengthOf(stream)
      (0 until totalCycles).flatMap(n => resolveCycle(List(stream), n)).toList
    }

  def generateInfiniteTimeline(tracks: List[Track])(using res: Resolvable[Pattern]): LazyList[ScheduledEvent] =
    def loop(nCycle: Int): LazyList[ScheduledEvent] =
      val sortedCycleEvents = resolveCycle(tracks, nCycle).sortBy(_.startTime.toDouble)
      LazyList.from(sortedCycleEvents) #::: loop(nCycle + 1)
    loop(0)

  private def resolveCycle(tracks: List[Track], nCycle: Int)(using res: Resolvable[Pattern]): List[ScheduledEvent] = {
    val startPhase = Fraction(nCycle)
    val endPhase = Fraction(nCycle + 1)

    tracks.flatMap { stream =>
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