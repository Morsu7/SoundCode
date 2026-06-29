package soundcode.engine

import soundcode.domain.*

class AudioPlayer(val cps: Double = 0.5) {

  private val cycleDurationMs: Double = 1000.0 / cps
  private val loopResolutionMs = 1L

  @volatile private var isRunning = false
  private var thread: Option[Thread] = None

  @volatile private var eventStream: LazyList[ScheduledEvent] = LazyList.empty

  @volatile private var firstTickTimeMs: Option[Long] = None

  def updateTimeline(stream: LazyList[ScheduledEvent]): Unit = this.eventStream = stream; this.firstTickTimeMs = None

  def start(): Unit = if (!isRunning) {
    isRunning = true
    thread = Some(new Thread(() => while (isRunning) {
      val now = System.currentTimeMillis()
      tick(AbsoluteTime(now))

      val sleepTime = loopResolutionMs - (System.currentTimeMillis() - now)
      if (sleepTime > 0) Thread.sleep(sleepTime)
    }))
    thread.get.start()
  }

  def stop(): Unit = isRunning = false; thread.foreach(_.join())

  def tick(now: AbsoluteTime): Unit = {
    if (firstTickTimeMs.isEmpty) firstTickTimeMs = Some(now.toLong)

    while (eventStream.nonEmpty) {
      val nextEvent = eventStream.head
      val expectedTriggerMs = firstTickTimeMs.get + (nextEvent.startTime.toDouble * cycleDurationMs).toLong

      if (now.toLong >= expectedTriggerMs) {
        eventStream = eventStream.tail
        val durationMs = Math.round((nextEvent.endTime.toDouble - nextEvent.startTime.toDouble) * cycleDurationMs)
        triggerSound(nextEvent.element, durationMs, nextEvent.appliedExtensions)
      } else {
        // L'evento futuro non è ancora pronto, usciamo dal tick
        return
      }
    }
  }

  protected def triggerSound(element: Element, durationMs: Long, extensions: List[Element]): Unit = {}
}