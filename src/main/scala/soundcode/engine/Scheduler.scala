package soundcode.engine

import soundcode.domain.*

trait Scheduler {
  def play(): Unit
  def updateTimeline(streams: List[Stream]): Unit
  def stop(): Unit

  def generateEvents(streams: List[Stream]): List[ScheduledEvent]
}

case class ScheduledEvent(startTime: Double, endTime: Double, element: Element, appliedExtensions: List[Element] = Nil)

class SchedulerImpl(bpm: Double) extends Scheduler {

  override def play(): Unit = ???

  override def updateTimeline(streams: List[Stream]): Unit = ???

  override def stop(): Unit = ???

  override def generateEvents(streams: List[Stream]): List[ScheduledEvent] = ???
}

