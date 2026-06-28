package soundcode.engine

import soundcode.domain.*

// Usiamo Contextual Abstraction (using) per iniettare lo scheduler puro e il risolutore
class AudioPlayer(val cps: Double = 0.5)(using scheduler: Scheduler, res: Resolvable[Pattern]) {

  private val cycleDurationMs: Long = (1000.0 / cps).toLong
  private val loopResolutionMs = 1L

  private case class PlayerState(cycleStart: Option[AbsoluteTime] = None, pending: List[(AbsoluteTime, ScheduledEvent)] = Nil, cycleCount: Int = 0)

  @volatile private var isRunning = false
  private var thread: Option[Thread] = None
  private var state = PlayerState()

  @volatile private var activeStreams: List[Stream] = Nil


  def updateTimeline(streams: List[Stream]): Unit = {
    this.activeStreams = streams
  }

  def start(): Unit = if (!isRunning) {
    isRunning = true
    state = PlayerState()
    thread = Some(new Thread(() => while (isRunning) {
      val startTime = System.currentTimeMillis()
      tick(AbsoluteTime(startTime))
      val sleepTime = loopResolutionMs - (System.currentTimeMillis() - startTime)
      if (sleepTime > 0) Thread.sleep(sleepTime)
    }))
    thread.get.start()
  }

  def stop(): Unit = { isRunning = false; thread.foreach(_.join()) }

  private def computeNextState(now: AbsoluteTime, s: PlayerState): (PlayerState, List[ScheduledEvent]) = {
    val stateWithNewCycle = s.cycleStart match {
      case Some(start) if (now.toLong - start.toLong) < cycleDurationMs => s
      case _ =>
        val newStart = s.cycleStart.map(st => AbsoluteTime(st.toLong + cycleDurationMs)).getOrElse(now)

        val newEvents = scheduler.generateEvents(activeStreams, s.cycleCount).map { e =>
          val triggerTimeMs = newStart.toLong + (e.startTime.toDouble * cycleDurationMs).toLong
          (AbsoluteTime(triggerTimeMs), e)
        }
        PlayerState(Some(newStart), s.pending ::: newEvents, s.cycleCount + 1)
    }

    val (ready, waiting) = stateWithNewCycle.pending.partition { case (time, _) => time <= now }
    (stateWithNewCycle.copy(pending = waiting), ready.map(_._2))
  }

  def tick(now: AbsoluteTime): Unit = {
    val (newState, eventsToPlay) = computeNextState(now, state)
    this.state = newState
    eventsToPlay.foreach { event =>
      val realDurationMs = Math.round((event.endTime.toDouble - event.startTime.toDouble) * cycleDurationMs)
      triggerSound(event.element, realDurationMs, event.appliedExtensions)
    }
  }

  protected def triggerSound(element: Element, durationMs: Long, extensions: List[Element]): Unit = {}
}