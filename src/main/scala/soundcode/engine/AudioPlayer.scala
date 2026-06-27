package soundcode.engine

import soundcode.domain.*

class AudioPlayer(val cps: Double = 0.5) {

  private val cycleDurationMs: Long = (1000.0 / cps).toLong
  private val loopResolutionMs = 1L

  private case class PlayerState(cycleStart: Option[Long] = None,
                                 pending: List[(Long, ScheduledEvent)] = Nil, cycleCount: Int = 0)

  @volatile private var isRunning = false
  private var thread: Option[Thread] = None
  private var state = PlayerState()

  def start(): Unit = if (!isRunning) {
    isRunning = true
    state = PlayerState()
    thread = Some(new Thread(() => while (isRunning) {
      val startTime = System.currentTimeMillis()
      tick(startTime)
      val sleepTime = loopResolutionMs - (System.currentTimeMillis() - startTime)
      if (sleepTime > 0) Thread.sleep(sleepTime)
    }))
    thread.get.start()
  }

  def stop(): Unit = isRunning = false; thread.foreach(_.join())

  private def computeNextState(now: Long, s: PlayerState): (PlayerState, List[ScheduledEvent]) = {
    val stateWithNewCycle = s.cycleStart match {
      case Some(start) if now - start < cycleDurationMs => s
      case _ =>
        val newStart = s.cycleStart.map(_ + cycleDurationMs).getOrElse(now)
        val newEvents = SchedulerImpl.generateEvents(s.cycleCount).map { e =>
          (newStart + (e.startTime * cycleDurationMs).toLong, e)
        }
        PlayerState(Some(newStart), s.pending ::: newEvents, s.cycleCount + 1)
    }
    val (ready, waiting) = stateWithNewCycle.pending.partition { case (time, _) => time <= now }
    (stateWithNewCycle.copy(pending = waiting), ready.map(_._2))
  }

  def tick(now: Long): Unit = {
    val (newState, eventsToPlay) = computeNextState(now, state)
    this.state = newState
    eventsToPlay.foreach { event =>
      val realDurationMs = Math.round((event.endTime - event.startTime) * cycleDurationMs).toLong
      triggerSound(event.element, realDurationMs, event.appliedExtensions)
    }
  }

  protected def triggerSound(element: Element, durationMs: Long, extensions: List[Element]): Unit = {}
}