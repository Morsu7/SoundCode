package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.{AggregationPattern, *}
import soundcode.domain.AggregationPattern.{AlternationPattern, SubPattern}

class EngineTest extends AnyFunSuite with Matchers {

  val pos = TextPosition(0, 0)

  // --- HELPER PER I TEST ---
  // Una rappresentazione super-compatta di un evento per rendere le asserzioni leggibili
  case class ExpEvent(element: String, start: Double, end: Double)

  // Funzione di utilità per mappare gli eventi reali in eventi di test
  def toExpEvents(events: List[ScheduledEvent]): List[ExpEvent] = {
    events.map { e =>
      e.element match {
        case Sound.SampleInText(s, _) => ExpEvent(s, e.startTime, e.endTime)
        case Sound.NoteInText(n, _)   => ExpEvent(n, e.startTime, e.endTime)
        case _                        => ExpEvent("unknown", e.startTime, e.endTime)
      }
    }
  }
  // --
  //
  // -----------------------

  test("Pattern semplice: bd hh sn hh") {
    val engine = new SchedulerImpl(120.0) // 1 ciclo = 2000 ms

    val basePattern: Pattern = List(
      Seq(
        Sound.SampleInText("bd", pos),
        Sound.SampleInText("hh", pos),
        Sound.SampleInText("sn", pos),
        Sound.SampleInText("hh", pos)
      )
    )

    val stream = Stream(base = basePattern, extensions = Nil)
    val events = engine.generateEvents(List(stream))

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

    val basePattern: Pattern = List(
      Seq(
        Sound.SampleInText("bd", pos),
        Sound.SampleInText("hh", pos)
      )
    )

    val notePattern: Pattern = List(
      Seq(
        Sound.NoteInText("c", pos),
        Sound.NoteInText("f", pos),
        Sound.NoteInText("g", pos)
      )
    )

    val stream = Stream(base = basePattern, extensions = List(notePattern))
    val events = engine.generateEvents(List(stream))

    events should have size 2

    val event1 = events(0)
    event1.element shouldBe Sound.SampleInText("bd", pos)
    // Al tempo 0ms, la nota attiva è la "c"
    event1.appliedExtensions should contain(Sound.NoteInText("c", pos))
    event1.appliedExtensions shouldNot contain(Sound.NoteInText("f", pos))

    val event2 = events(1)
    event2.element shouldBe Sound.SampleInText("hh", pos)
    // Al tempo 1000ms, la nota attiva è la "f"
    event2.appliedExtensions should contain(Sound.NoteInText("f", pos))
    event2.appliedExtensions shouldNot contain(Sound.NoteInText("g", pos))
  }

  test("Pattern complesso (Poliritmia): bd hh sn hh [hh , sn < bd hh > ]") {
    val engine = new SchedulerImpl(120.0) // 1 ciclo = 2000 ms (5 step da 400ms)

    val basePattern: Pattern = List(
      Seq(
        Sound.SampleInText("bd", pos),
        Sound.SampleInText("hh", pos),
        Sound.SampleInText("sn", pos),
        Sound.SampleInText("hh", pos),
        AggregationPattern.SubPattern(List(
          Seq(
            // Layer 1: hh
            Sound.SampleInText("hh", pos)
          ),
          Seq(
            // Layer 2: sn seguito da un'alternanza bd/hh
            Sound.SampleInText("sn", pos),
            AggregationPattern.AlternationPattern(List(
              Seq(
                Sound.SampleInText("bd", pos),
                Sound.SampleInText("hh", pos)
              )
            ))
          )
        ))
      )
    )

    val stream = Stream(base = basePattern, extensions = Nil)
    val events = engine.generateEvents(List(stream))

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