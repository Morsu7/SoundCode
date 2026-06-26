package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.*
import soundcode.engine.SchedulerImpl

class SchedulerTest extends AnyFunSuite with Matchers {

  //--------------------------UTILITY-----------------------------------
  val pos = TextPosition(0, 0)

  case class ExpEvent(element: String, start: Double, end: Double, extensions: List[String] = Nil)

  // Utility per arrotondare i Double a 3 cifre decimali
  def round3(d: Double): Double = Math.round(d * 1000.0) / 1000.0

  def toExpEvents(events: List[ScheduledEvent]): List[ExpEvent] = {
    events.map { e =>
      val extStrings = e.appliedExtensions.map {
        case Sound.NoteInText(n, _) => n
        case Sound.SampleInText(s, _) => s
        case _ => "unknown"
      }

      val rStart = round3(e.startTime)
      val rEnd = round3(e.endTime)

      e.element match {
        case Sound.SampleInText(s, _) => ExpEvent(s, rStart, rEnd, extStrings)
        case Sound.NoteInText(n, _) => ExpEvent(n, rStart, rEnd, extStrings)
        case _ => ExpEvent("unknown", rStart, rEnd, extStrings)
      }
    }
  }
  //-----------------------------------------------------------------------------

  test("bd hh sn hh") {

    val stream = Stream(base = Patterns.`bd Hh Sn Hh`, extensions = Nil)
    SchedulerImpl.updateTimeline(List(stream))
    val events = SchedulerImpl.generateEvents(0)

    events should have size 4

    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.25, List()),
      ExpEvent("hh", 0.25, 0.5, List()),
      ExpEvent("sn", 0.5, 0.75, List()),
      ExpEvent("hh", 0.75, 1.0, List())
    )
  }

  test("sound(\"bd hh\").note(\"c f g\")") {

    val stream = Stream(base = Patterns.`bd Hh`, extensions = List(Patterns.`c f g`))

    SchedulerImpl.updateTimeline(List(stream))
    val events = SchedulerImpl.generateEvents(0)

    events should have size 2

    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.5, List("c")),
      ExpEvent("hh", 0.5, 1, List("f"))
    )
  }

  test("bd hh sn hh [hh , sn < bd hh > ]") {

    val stream = Stream(base = Patterns.`bd hh sn hh [hh , sn < bd hh > ]`, extensions = Nil)
    SchedulerImpl.updateTimeline(List(stream))
    val firstCycle = SchedulerImpl.generateEvents(0)

    firstCycle should have size 7

    toExpEvents(firstCycle) should contain allOf(
      // Step 1-4
      ExpEvent("bd", 0.0, 0.2),
      ExpEvent("hh", 0.2, 0.4),
      ExpEvent("sn", 0.4, 0.6),
      ExpEvent("hh", 0.6, 0.8),

      // Step 5: SubPattern
      ExpEvent("hh", 0.8, 1),
      ExpEvent("sn", 0.8, 0.9),
      ExpEvent("bd", 0.9, 1)
    )

    val secondCycle = SchedulerImpl.generateEvents(1)

    secondCycle should have size 7

    toExpEvents(secondCycle) should contain allOf(
      // Step 1-4
      ExpEvent("bd", 0.0, 0.2),
      ExpEvent("hh", 0.2, 0.4),
      ExpEvent("sn", 0.4, 0.6),
      ExpEvent("hh", 0.6, 0.8),

      // Step 5: SubPattern
      ExpEvent("hh", 0.8, 1),
      ExpEvent("sn", 0.8, 0.9),
      ExpEvent("hh", 0.9, 1)
    )
  }

  test("bd [hh [sn cp]]") {
    val stream = Stream(base = Patterns.`bd [hh [sn cp]]`, extensions = Nil)
    SchedulerImpl.updateTimeline(List(stream))
    val events = SchedulerImpl.generateEvents(0)

    events should have size 4

    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.5), // Metà ciclo
      ExpEvent("hh", 0.5, 0.75), // Un quarto di ciclo
      ExpEvent("sn", 0.75, 0.875), // Un ottavo
      ExpEvent("cp", 0.875, 1.0) // Un ottavo
    )
  }

  test("<bd [hh sn]> cp") {
    val stream = Stream(base = Patterns.`<bd [hh sn]> cp`, extensions = Nil)
    SchedulerImpl.updateTimeline(List(stream))

    // GIRO 0: Sceglie 'bd' (1 elemento). Diviso tra Alternanza e 'cp'
    val eventsGiro0 = SchedulerImpl.generateEvents(0)
    toExpEvents(eventsGiro0) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.5),
      ExpEvent("cp", 0.5, 1.0)
    )

    // GIRO 1: Sceglie 'hh sn' (2 elementi). Il primo 0.5 viene diviso a metà!
    val eventsGiro1 = SchedulerImpl.generateEvents(1)
    toExpEvents(eventsGiro1) should contain theSameElementsInOrderAs List(
      ExpEvent("hh", 0.0, 0.25),
      ExpEvent("sn", 0.25, 0.5),
      ExpEvent("cp", 0.5, 1.0)
    )
  }

  test("[bd hh, cp cp cp]") {
    val stream = Stream(base = Patterns.`[bd hh, cp cp cp]`, extensions = Nil)
    SchedulerImpl.updateTimeline(List(stream))
    val events = SchedulerImpl.generateEvents(0)

    events should have size 5

    // Il primo layer si divide in due (0.0-0.5, 0.5-1.0)
    // Il secondo si divide in tre (0.0-0.333, 0.333-0.667, 0.667-1.0)
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
    SchedulerImpl.updateTimeline(List(stream))
    val events = SchedulerImpl.generateEvents(0)

    events should have size 3

    // La base suona a: 0.0, 0.333, 0.666
    // L'estensione cambia a: 0.5
    // Quindi 'bd' e 'sn' prendono "c", 'hh' prende "f"!
    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.333, List("c")),
      ExpEvent("sn", 0.333, 0.667, List("c")),
      ExpEvent("hh", 0.667, 1.0, List("f"))
    )
  }

  test("bd hh sn hh < bd hh , hh , hh >") {
    val stream = Stream(base = Patterns.`bd hh sn hh < bd hh , hh , hh >`, extensions = Nil)
    SchedulerImpl.updateTimeline(List(stream))

    val eventsGiro0 = SchedulerImpl.generateEvents(0)

    eventsGiro0 should have size 7 // 4 note base + 3 in parallelo alla fine
    toExpEvents(eventsGiro0) should contain theSameElementsAs List(
      ExpEvent("bd", 0.0, 0.2),
      ExpEvent("hh", 0.2, 0.4),
      ExpEvent("sn", 0.4, 0.6),
      ExpEvent("hh", 0.6, 0.8),
      ExpEvent("bd", 0.8, 1.0),
      ExpEvent("hh", 0.8, 1.0),
      ExpEvent("hh", 0.8, 1.0)
    )

    // GIRO 1: bd hh sn hh | [hh, hh, hh] in parallelo (da 0.8 a 1.0)
    val eventsGiro1 = SchedulerImpl.generateEvents(1)

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