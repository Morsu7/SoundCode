package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.*
import soundcode.engine.Resolvable.given
import soundcode.interpreter.interpret

class AudioPlayerTest extends AnyFunSuite with Matchers {

  given Scheduler = SchedulerImpl

  test("bd hh sn hh") {
    val player = setupPlayer(interpret("sound(\"bd hh sn hh\")"))
    val events = playCycle(player, 0)

    events should have size 4
    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 500L),
      PlayedEvent("hh", 500L, 500L),
      PlayedEvent("sn", 1000L, 500L),
      PlayedEvent("hh", 1500L, 500L)
    )
  }

  test("sound(\"bd hh\").note(\"c f g\")") {
    val player = setupPlayer(interpret("sound(\"bd hh\").note(\"c f g\")"))
    val events = playCycle(player, 0)

    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 1000L, List("c4")),
      PlayedEvent("hh", 1000L, 1000L, List("f4"))
    )
  }

  test("bd hh sn hh [hh , sn < bd hh > ] (Due Cicli)") {
    val player = setupPlayer(interpret("sound(\"bd hh sn hh [hh , sn < bd hh > ]\")"))

    // --- CICLO 0 ---
    val cycle0 = playCycle(player, 0)
    cycle0 should have size 7
    cycle0 should contain theSameElementsAs List(
      PlayedEvent("bd", 0L, 400L),
      PlayedEvent("hh", 400L, 400L),
      PlayedEvent("sn", 800L, 400L),
      PlayedEvent("hh", 1200L, 400L),
      PlayedEvent("hh", 1600L, 400L),
      PlayedEvent("sn", 1600L, 200L),
      PlayedEvent("bd", 1800L, 200L)
    )

    val cycle1 = playCycle(player, 1)
    cycle1 should have size 7
    cycle1 should contain theSameElementsAs List(
      PlayedEvent("bd", 2000L, 400L),
      PlayedEvent("hh", 2400L, 400L),
      PlayedEvent("sn", 2800L, 400L),
      PlayedEvent("hh", 3200L, 400L),
      PlayedEvent("hh", 3600L, 400L),
      PlayedEvent("sn", 3600L, 200L),
      PlayedEvent("hh", 3800L, 200L)
    )
  }

  test("Scalabilità del tempo: cps = 1.0 (1 secondo per ciclo)") {
    val myCps = 1.0
    val player = setupPlayer(interpret("sound(\"bd hh sn hh\")"), cps = myCps)
    val events = playCycle(player, 0, myCps)

    events should have size 4
    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 250L),
      PlayedEvent("hh", 250L, 250L),
      PlayedEvent("sn", 500L, 250L),
      PlayedEvent("hh", 750L, 250L)
    )
  }

  test("Scalabilità estrema: cps = 2.0 (Mezzo secondo per ciclo)") {
    val myCps = 2.0
    val player = setupPlayer(interpret("sound(\"bd hh sn hh\")"), cps = myCps)
    val events = playCycle(player, 0, myCps)

    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 125L),
      PlayedEvent("hh", 125L, 125L),
      PlayedEvent("sn", 250L, 125L),
      PlayedEvent("hh", 375L, 125L)
    )
  }

  test("Inversione: base Note e estensione Sample (note(\"c f\").sound(\"bd\"))") {
    val pos = TextPosition(0, 0)
    val player = setupPlayer(interpret("note(\"c f\").sound(\"bd hh sn hh\")"))
    val events = playCycle(player, 0)

    events should contain theSameElementsInOrderAs List(
      PlayedEvent("c4", 0L, 1000L, List("bd")),
      PlayedEvent("f4", 1000L, 1000L, List("sn"))
    )
  }

  test("Propagazione degli Effetti (Gain e Room)") {
    val pos = TextPosition(0, 0)
    val player = setupPlayer(interpret("sound(\"bd sn hh\").gain(\"3 5\").room(\"6\")"))
    val events = playCycle(player, 0)

    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 667L, List("gain(3.0)", "room(6.0)")),
      PlayedEvent("sn", 666L, 667L, List("gain(3.0)", "room(6.0)")),
      PlayedEvent("hh", 1333L, 667L, List("gain(5.0)", "room(6.0)"))
    )
  }

  case class PlayedEvent(name: String, triggerTimeMs: Long, durationMs: Long, extensions: List[String] = Nil)

  class TestableAudioPlayer(cps: Double)(using Scheduler, Resolvable[Pattern]) extends AudioPlayer(cps) {
    var playedEvents: List[PlayedEvent] = Nil
    var simulatedNow: AbsoluteTime = AbsoluteTime(0L)

    override def tick(now: AbsoluteTime): Unit = {
      simulatedNow = now
      super.tick(now)
    }

    override protected def triggerSound(element: Element, durationMs: Long, extensions: List[Element]): Unit = {
      // Usiamo .value per estrarre la stringa dall'Opaque Type
      val name = element match {
        case Sound.SampleInText(s, _) => s.value
        case Sound.NoteInText(n, _) => n.value
        case _ => "unknown"
      }
      val extNames = extensions.map {
        case Sound.SampleInText(s, _) => s.value
        case Sound.NoteInText(n, _) => n.value
        case Effect.Gain(v) => s"gain($v)"
        case Effect.Room(v) => s"room($v)"
        case _ => "unknown"
      }
      playedEvents = playedEvents :+ PlayedEvent(name, simulatedNow.toLong, durationMs, extNames)
    }
  }

  def setupPlayer(streams: List[Stream], cps: Double = 0.5): TestableAudioPlayer = {
    val player = new TestableAudioPlayer(cps)
    player.updateTimeline(streams)
    player
  }

  def playCycle(player: TestableAudioPlayer, cycleIndex: Int, cps: Double = 0.5): List[PlayedEvent] = {
    val cycleDurationMs = (1000.0 / cps).toLong
    val startMs = cycleIndex * cycleDurationMs
    val endMs = startMs + cycleDurationMs - 1
    for (t <- startMs to endMs) {
      player.tick(AbsoluteTime(t))
    }
    val events = player.playedEvents
    player.playedEvents = Nil
    events
  }
}