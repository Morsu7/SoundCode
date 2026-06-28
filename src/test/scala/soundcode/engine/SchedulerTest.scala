package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.*

// Importiamo i given necessari
import soundcode.engine.Resolvable.given

class SchedulerTest extends AnyFunSuite with Matchers {

  val pos = TextPosition(0, 0)
  case class ExpEvent(element: String, start: Double, end: Double, extensions: List[String] = Nil)

  def round3(d: Double): Double = Math.round(d * 1000.0) / 1000.0

  def toExpEvents(events: List[ScheduledEvent]): List[ExpEvent] = {
    events.map { e =>
      val extStrings = e.appliedExtensions.map {
        // Estraiamo il valore dalle opaque type
        case Sound.NoteInText(n, _) => n.value
        case Sound.SampleInText(s, _) => s.value
        case Effect.Gain(v) => v.toString
        case Effect.Room(v) => v.toString
        case _ => "unknown"
      }

      // Convertiamo Phase in Double per il test
      val rStart = round3(e.startTime.toDouble)
      val rEnd = round3(e.endTime.toDouble)

      e.element match {
        case Sound.SampleInText(s, _) => ExpEvent(s.value, rStart, rEnd, extStrings)
        case Sound.NoteInText(n, _) => ExpEvent(n.value, rStart, rEnd, extStrings)
        case _ => ExpEvent("unknown", rStart, rEnd, extStrings)
      }
    }
  }

  test("bd hh sn hh") {
    val stream = Stream(base = Patterns.`bd Hh Sn Hh`, extensions = Nil)

    val events = SchedulerImpl.generateEvents(List(stream), 0)

    events should have size 4
    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.25, List()),
      ExpEvent("hh", 0.25, 0.5, List()),
      ExpEvent("sn", 0.5, 0.75, List()),
      ExpEvent("hh", 0.75, 1.0, List())
    )
  }

  test("<bd bd hh bd rim bd hh bd>") {
    val stream = Stream(base = Patterns.`<bd bd hh bd rim bd hh bd>`, extensions = Nil)
    val expectedOutcomes = List(
      List(ExpEvent("bd", 0.0, 1.0)),
      List(ExpEvent("bd", 0.0, 1.0)),
      List(ExpEvent("hh", 0.0, 1.0)),
      List(ExpEvent("bd", 0.0, 1.0)),
      List(ExpEvent("rim", 0.0, 1.0)),
      List(ExpEvent("bd", 0.0, 1.0)),
      List(ExpEvent("hh", 0.0, 1.0)),
      List(ExpEvent("bd", 0.0, 1.0))
    )

    for (cycleIndex <- 0 until 8) {
      val events = SchedulerImpl.generateEvents(List(stream), cycleIndex)
      events should have size 1
      toExpEvents(events) should contain theSameElementsInOrderAs expectedOutcomes(cycleIndex)
    }
  }

  test("note(c f [ g h c# ]).sound(<bd [hh sn]> cp).room(4 5 [4] , <4 5 6>)") {
    val stream = Stream(
      base = Patterns.`c f [ g h c# ]`,
      extensions = List(
        Patterns.`<bd [hh sn]> cp`,
        Patterns.`room(4 5 [4] , <4 5 6>)`
      )
    )

    val events = SchedulerImpl.generateEvents(List(stream), 0)
    events should not be empty
    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("c", 0.0, 0.333, List("bd","4","4")),
      ExpEvent("f", 0.333, 0.667, List("bd","5","4")),
      ExpEvent("g", 0.667, 0.778, List("cp","4","4")),
      ExpEvent("h", 0.778, 0.889, List("cp","4","4")),
      ExpEvent("c#", 0.889, 1, List("cp","4","4"))
    )

    val events1 = SchedulerImpl.generateEvents(List(stream), 1)
    events should not be empty
    toExpEvents(events1) should contain theSameElementsInOrderAs List(
      ExpEvent("c", 0.0, 0.333, List("hh", "4", "5")),
      ExpEvent("f", 0.333, 0.667, List("sn", "5", "5")),
      ExpEvent("g", 0.667, 0.778, List("cp", "4", "5")),
      ExpEvent("h", 0.778, 0.889, List("cp", "4", "5")),
      ExpEvent("c#", 0.889, 1, List("cp", "4", "5"))
    )
  }

  test("sound(\"bd hh\").note(\"c f g\")") {
    val stream = Stream(base = Patterns.`bd Hh`, extensions = List(Patterns.`c f g`))
    val events = SchedulerImpl.generateEvents(List(stream), 0)

    events should have size 2
    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.5, List("c")),
      ExpEvent("hh", 0.5, 1, List("f"))
    )
  }

  test("bd hh sn hh [hh , sn < bd hh > ]") {
    val stream = Stream(base = Patterns.`bd hh sn hh [hh , sn < bd hh > ]`, extensions = Nil)
    val firstCycle = SchedulerImpl.generateEvents(List(stream), 0)

    firstCycle should have size 7
    toExpEvents(firstCycle) should contain allOf(
      ExpEvent("bd", 0.0, 0.2),
      ExpEvent("hh", 0.2, 0.4),
      ExpEvent("sn", 0.4, 0.6),
      ExpEvent("hh", 0.6, 0.8),
      ExpEvent("hh", 0.8, 1),
      ExpEvent("sn", 0.8, 0.9),
      ExpEvent("bd", 0.9, 1)
    )

    val secondCycle = SchedulerImpl.generateEvents(List(stream), 1)
    secondCycle should have size 7
    toExpEvents(secondCycle) should contain allOf(
      ExpEvent("bd", 0.0, 0.2),
      ExpEvent("hh", 0.2, 0.4),
      ExpEvent("sn", 0.4, 0.6),
      ExpEvent("hh", 0.6, 0.8),
      ExpEvent("hh", 0.8, 1),
      ExpEvent("sn", 0.8, 0.9),
      ExpEvent("hh", 0.9, 1)
    )
  }

  test("bd [hh [sn cp]]") {
    val stream = Stream(base = Patterns.`bd [hh [sn cp]]`, extensions = Nil)
    val events = SchedulerImpl.generateEvents(List(stream), 0)

    events should have size 4
    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.5),
      ExpEvent("hh", 0.5, 0.75),
      ExpEvent("sn", 0.75, 0.875),
      ExpEvent("cp", 0.875, 1.0)
    )
  }

  test("<bd [hh sn]> cp") {
    val stream = Stream(base = Patterns.`<bd [hh sn]> cp`, extensions = Nil)
    val eventsGiro0 = SchedulerImpl.generateEvents(List(stream), 0)

    toExpEvents(eventsGiro0) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.5),
      ExpEvent("cp", 0.5, 1.0)
    )

    val eventsGiro1 = SchedulerImpl.generateEvents(List(stream), 1)
    toExpEvents(eventsGiro1) should contain theSameElementsInOrderAs List(
      ExpEvent("hh", 0.0, 0.25),
      ExpEvent("sn", 0.25, 0.5),
      ExpEvent("cp", 0.5, 1.0)
    )
  }

  test("[bd hh, cp cp cp]") {
    val stream = Stream(base = Patterns.`[bd hh, cp cp cp]`, extensions = Nil)
    val events = SchedulerImpl.generateEvents(List(stream), 0)

    events should have size 5
    toExpEvents(events) should contain allOf(
      ExpEvent("bd", 0.0, 0.5),
      ExpEvent("hh", 0.5, 1.0),
      ExpEvent("cp", 0.0, 0.333),
      ExpEvent("cp", 0.333, 0.667),
      ExpEvent("cp", 0.667, 1.0)
    )
  }

  test("sound(\"bd sn hh\").note(\"c f\")") {
    val stream = Stream(base = Patterns.`bd sn hh`, extensions = List(Patterns.`c f`))
    val events = SchedulerImpl.generateEvents(List(stream), 0)

    events should have size 3
    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.333, List("c")),
      ExpEvent("sn", 0.333, 0.667, List("c")),
      ExpEvent("hh", 0.667, 1.0, List("f"))
    )
  }

  test("bd hh sn hh < bd hh , hh , hh >") {
    val stream = Stream(base = Patterns.`bd hh sn hh < bd hh , hh , hh >`, extensions = Nil)
    val eventsGiro0 = SchedulerImpl.generateEvents(List(stream), 0)

    eventsGiro0 should have size 7
    toExpEvents(eventsGiro0) should contain theSameElementsAs List(
      ExpEvent("bd", 0.0, 0.2),
      ExpEvent("hh", 0.2, 0.4),
      ExpEvent("sn", 0.4, 0.6),
      ExpEvent("hh", 0.6, 0.8),
      ExpEvent("bd", 0.8, 1.0),
      ExpEvent("hh", 0.8, 1.0),
      ExpEvent("hh", 0.8, 1.0)
    )

    val eventsGiro1 = SchedulerImpl.generateEvents(List(stream), 1)
    eventsGiro1 should have size 7
    toExpEvents(eventsGiro1) should contain theSameElementsAs List(
      ExpEvent("bd", 0.0, 0.2),
      ExpEvent("hh", 0.2, 0.4),
      ExpEvent("sn", 0.4, 0.6),
      ExpEvent("hh", 0.6, 0.8),
      ExpEvent("hh", 0.8, 1.0),
      ExpEvent("hh", 0.8, 1.0),
      ExpEvent("hh", 0.8, 1.0)
    )
  }
}