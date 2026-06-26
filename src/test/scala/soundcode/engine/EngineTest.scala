package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.{AggregationPattern, *}
import soundcode.domain.AggregationPattern.{AlternationPattern, SubPattern}

class EngineTest extends AnyFunSuite with Matchers {
  //--------------------------UTILITY-----------------------------------
  val pos = TextPosition(0, 0)

  case class ExpEvent(element: String, start: Double, end: Double, extensions: List[String] = Nil)

  def toExpEvents(events: List[ScheduledEvent]): List[ExpEvent] = {
    events.map { e =>
      val extStrings = e.appliedExtensions.map {
        case Sound.NoteInText(n, _) => n
        case Sound.SampleInText(s, _) => s
        case _ => "unknown"
      }

      e.element match {
        case Sound.SampleInText(s, _) => ExpEvent(s, e.startTime, e.endTime, extStrings)
        case Sound.NoteInText(n, _) => ExpEvent(n, e.startTime, e.endTime, extStrings)
        case _ => ExpEvent("unknown", e.startTime, e.endTime, extStrings)
      }
    }
  }
  //-----------------------------------------------------------------------------

  test("Pattern semplice: bd hh sn hh") {
    val engine = new SchedulerImpl(120.0) // 1 ciclo = 2000 ms

    val stream = Stream(base = Patterns.`bd Hh Sn Hh`, extensions = Nil)
    engine.updateTimeline(List(stream))
    val events = engine.generateEvents()

    events should have size 4

    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 500.0),
      ExpEvent("hh", 500.0, 1000.0),
      ExpEvent("sn", 1000.0, 1500.0),
      ExpEvent("hh", 1500.0, 2000.0)
    )
  }

  test("Pattern con estensioni: sound(\"bd hh\").note(\"c f g\")") {
    val engine = new SchedulerImpl(120.0) // 1 ciclo = 2000 ms

    val stream = Stream(base = Patterns.`bd Hh`, extensions = List(Patterns.`c f g`))

    engine.updateTimeline(List(stream))
    val events = engine.generateEvents()

    events should have size 2

    toExpEvents(events) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0.0, 1000.0, List("c")),
      ExpEvent("hh", 1000.0, 2000.0, List("f"))
    )
  }

  test("Pattern complesso (Poliritmia): bd hh sn hh [hh , sn < bd hh > ]") {
    val engine = new SchedulerImpl(120.0) // 1 ciclo = 2000 ms (5 step da 400ms)

    val stream = Stream(base = Patterns.`bd hh sn hh [hh , sn < bd hh > ]`, extensions = Nil)
    engine.updateTimeline(List(stream))
    val events = engine.generateEvents()

    events should have size 7

    toExpEvents(events) should contain allOf (
      // Step 1-4
      ExpEvent("bd", 0.0, 400.0),
      ExpEvent("hh", 400.0, 800.0),
      ExpEvent("sn", 800.0, 1200.0),
      ExpEvent("hh", 1200.0, 1600.0),

      // Step 5: SubPattern
      ExpEvent("hh", 1600.0, 2000.0), // Layer 1 (dura tutto il 5° step)
      ExpEvent("sn", 1600.0, 1800.0), // Layer 2 - Prima metà
      ExpEvent("bd", 1800.0, 2000.0)  // Layer 2 - Seconda metà (Alternation al 1° ciclo)
    )
  }
}


object Patterns {
  import soundcode.domain.AggregationPattern.*

  val pos = TextPosition(0, 0)

  val `bd Hh Sn Hh`: Pattern = List(
    Seq(
      Sound.SampleInText("bd", pos),
      Sound.SampleInText("hh", pos),
      Sound.SampleInText("sn", pos),
      Sound.SampleInText("hh", pos)
    )
  )

  val `bd Hh`: Pattern = List(
    Seq(
      Sound.SampleInText("bd", pos),
      Sound.SampleInText("hh", pos)
    )
  )

  val `c f g`: Pattern = List(
    Seq(
      Sound.NoteInText("c", pos),
      Sound.NoteInText("f", pos),
      Sound.NoteInText("g", pos)
    )
  )

  val `bd hh sn hh [hh , sn < bd hh > ]`: Pattern = List(
    Seq(
      Sound.SampleInText("bd", pos),
      Sound.SampleInText("hh", pos),
      Sound.SampleInText("sn", pos),
      Sound.SampleInText("hh", pos),
      SubPattern(List(
        Seq(
          Sound.SampleInText("hh", pos)
        ),
        Seq(
          Sound.SampleInText("sn", pos),
          AlternationPattern(List(
            Seq(
              Sound.SampleInText("bd", pos),
              Sound.SampleInText("hh", pos)
            )
          ))
        )
      ))
    )
  )
}