package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.*
import soundcode.interpreter.interpret
import soundcode.engine.Resolvable.given

class SchedulerTest extends AnyFunSuite with Matchers {

  def resolve(streams: List[Stream], cycle: Int): List[ExpEvent] =
    SchedulerImpl.generateInfiniteTimeline(streams)
      .dropWhile(_.startTime.toDouble < cycle.toDouble)
      .takeWhile(_.startTime.toDouble < (cycle.toDouble + 1.0))
      .map(_.toExp)
      .toList

  test("bd hh sn hh") {
    val streams = interpret("sound(\"bd hh sn hh\")")

    resolve(streams, 0) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.25),
      ExpEvent("hh", 0.25, 0.5),
      ExpEvent("sn", 0.5, 0.75),
      ExpEvent("hh", 0.75, 1.0)
    )
  }

  test("<bd bd hh bd rim bd hh bd>") {
    val streams = interpret("sound(\"<bd bd hh bd rim bd hh bd>\")")
    val outcomes = List("bd", "bd", "hh", "bd", "rim", "bd", "hh", "bd")

    for (cycleIndex <- 0 until 8) {
      resolve(streams, cycleIndex) should contain theSameElementsInOrderAs List(
        ExpEvent(outcomes(cycleIndex), cycleIndex.toDouble, cycleIndex.toDouble + 1.0)
      )
    }
  }

  test("note(c f [ g c c# ]).sound(<bd [hh sn]> cp).room(4 5 [4] , <4 5 6>)") {
    val streams = interpret("note(\"c f [ g c c# ]\").sound(\"<bd [hh sn]> cp\").room(\"4 5 [4] , <4 5 6>\")")

    resolve(streams, 0) should contain theSameElementsInOrderAs List(
      ExpEvent("c4", 0.0, 0.333, List("bd", "4.0", "4.0")),
      ExpEvent("f4", 0.333, 0.667, List("bd", "5.0", "4.0")),
      ExpEvent("g4", 0.667, 0.778, List("cp", "4.0", "4.0")),
      ExpEvent("c4", 0.778, 0.889, List("cp", "4.0", "4.0")),
      ExpEvent("c#4", 0.889, 1.0, List("cp", "4.0", "4.0"))
    )

    resolve(streams, 1) should contain theSameElementsInOrderAs List(
      ExpEvent("c4", 1.0, 1.333, List("hh", "4.0", "5.0")),
      ExpEvent("f4", 1.333, 1.667, List("sn", "5.0", "5.0")),
      ExpEvent("g4", 1.667, 1.778, List("cp", "4.0", "5.0")),
      ExpEvent("c4", 1.778, 1.889, List("cp", "4.0", "5.0")),
      ExpEvent("c#4", 1.889, 2.0, List("cp", "4.0", "5.0"))
    )
  }

  test("bd hh sn hh [hh , sn < bd hh > ]") {
    val streams = interpret("sound(\"bd hh sn hh [hh , sn < bd hh > ]\")")

    resolve(streams, 0) should contain allOf(
      ExpEvent("bd", 0.0, 0.2),
      ExpEvent("hh", 0.2, 0.4),
      ExpEvent("sn", 0.4, 0.6),
      ExpEvent("hh", 0.6, 0.8),
      ExpEvent("hh", 0.8, 1.0),
      ExpEvent("sn", 0.8, 0.9),
      ExpEvent("bd", 0.9, 1.0)
    )

    resolve(streams, 1) should contain allOf(
      ExpEvent("bd", 1.0, 1.2),
      ExpEvent("hh", 1.2, 1.4),
      ExpEvent("sn", 1.4, 1.6),
      ExpEvent("hh", 1.6, 1.8),
      ExpEvent("hh", 1.8, 2.0),
      ExpEvent("sn", 1.8, 1.9),
      ExpEvent("hh", 1.9, 2.0)
    )
  }

  test("<bd [hh sn]> cp") {
    val streams = interpret("sound(\"<bd [hh sn]> cp\")")

    resolve(streams, 0) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 0.5),
      ExpEvent("cp", 0.5, 1.0)
    )

    resolve(streams, 1) should contain theSameElementsInOrderAs List(
      ExpEvent("hh", 1.0, 1.25),
      ExpEvent("sn", 1.25, 1.5),
      ExpEvent("cp", 1.5, 2.0)
    )
  }

  test("generateBoundedTimelines rispetta le lunghezze dei pattern") {
    val streams = interpret("sound(\"bd hh sn hh\")\nsound(\"<cp rim bd>\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 2

    // Il secondo stream deve fermarsi alla Phase 3.0 (3 cicli)
    viewData(1).last.endTime.toDouble shouldBe 3.0

    viewData(1).map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("cp", 0.0, 1.0),
      ExpEvent("rim", 1.0, 2.0),
      ExpEvent("bd", 2.0, 3.0)
    )
  }
}

case class ExpEvent(element: String, start: Double, end: Double, extensions: List[String] = Nil)

extension (e: ScheduledEvent)
  def toExp: ExpEvent =
    val round = (d: Double) => Math.round(d * 1000.0) / 1000.0
    val exts = e.appliedExtensions.map {
      case Sound.NoteInText(n, _) => n.value
      case Sound.SampleInText(s, _) => s.value
      case Effect.Gain(v) => v.toString
      case Effect.Room(v) => v.toString
      case _ => "unknown"
    }
    val name = e.element match {
      case Sound.SampleInText(s, _) => s.value
      case Sound.NoteInText(n, _) => n.value
      case _ => "unknown"
    }
    ExpEvent(name, round(e.startTime.toDouble), round(e.endTime.toDouble), exts)