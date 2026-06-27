package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.*

class AudioPlayerTest extends AnyFunSuite with Matchers {

  test("bd hh sn hh") {
    val player = setupPlayer(Patterns.`bd Hh Sn Hh`)
    val events = playCycle(player, 0) // Niente più 1999L a mano!

    events should have size 4
    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 500L),
      PlayedEvent("hh", 500L, 500L),
      PlayedEvent("sn", 1000L, 500L),
      PlayedEvent("hh", 1500L, 500L)
    )
  }

  test("sound(\"bd hh\").note(\"c f g\")") {
    val player = setupPlayer(Patterns.`bd Hh`, List(Patterns.`c f g`))
    val events = playCycle(player, 0)

    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 1000L, List("c")),
      PlayedEvent("hh", 1000L, 1000L, List("f"))
    )
  }

  test("bd hh sn hh [hh , sn < bd hh > ] (Due Cicli)") {
    val player = setupPlayer(Patterns.`bd hh sn hh [hh , sn < bd hh > ]`)

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
    // A cps = 1.0, il ciclo dura esattamente 1000ms.
    val myCps = 1.0
    val player = setupPlayer(Patterns.`bd Hh Sn Hh`, cps = myCps)
    val events = playCycle(player, 0, myCps)

    events should have size 4
    // Le note durano 250ms l'una (1000 / 4) invece di 500ms!
    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 250L),
      PlayedEvent("hh", 250L, 250L),
      PlayedEvent("sn", 500L, 250L),
      PlayedEvent("hh", 750L, 250L)
    )
  }

  test("Scalabilità estrema: cps = 2.0 (Mezzo secondo per ciclo)") {
    // A cps = 2.0, il ciclo dura 500ms. Modalità "Breakcore"!
    val myCps = 2.0
    val player = setupPlayer(Patterns.`bd Hh Sn Hh`, cps = myCps)
    val events = playCycle(player, 0, myCps)

    // Le note durano 125ms l'una (500 / 4)
    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 125L),
      PlayedEvent("hh", 125L, 125L),
      PlayedEvent("sn", 250L, 125L),
      PlayedEvent("hh", 375L, 125L)
    )
  }

  test("Inversione: base Note e estensione Sample (note(\"c f\").sound(\"bd\"))") {
    val pos = TextPosition(0, 0)

    val player = setupPlayer(base = Patterns.`c f`, extensions = List(Patterns.`bd sn hh`))
    val events = playCycle(player, 0)

    events should contain theSameElementsInOrderAs List(
      PlayedEvent("c", 0L, 1000L, List("bd")),
      PlayedEvent("f", 1000L, 1000L, List("sn"))
    )
  }

  test("Propagazione degli Effetti (Gain e Room)") {
    val pos = TextPosition(0, 0)

    val player = setupPlayer(Patterns.`bd sn hh`, extensions = List(Patterns.`gain 3 5`, Patterns.`room 6`))
    val events = playCycle(player, 0)

    // Entrambi i sample devono ricevere contemporaneamente gain e room!
    events should contain theSameElementsInOrderAs List(
      PlayedEvent("bd", 0L, 667L, List("gain(3)", "room(6)")),
      PlayedEvent("sn", 666L, 667L, List("gain(3)", "room(6)")),
      PlayedEvent("hh", 1333L, 667L, List("gain(5)", "room(6)"))
    )
  }


  case class PlayedEvent(name: String, triggerTimeMs: Long, durationMs: Long, extensions: List[String] = Nil)

  class TestableAudioPlayer(cps: Double) extends AudioPlayer(cps) {
    var playedEvents: List[PlayedEvent] = Nil
    var simulatedNow: Long = 0L

    override def tick(now: Long): Unit = {
      simulatedNow = now
      super.tick(now)
    }

    override protected def triggerSound(element: Element, durationMs: Long, extensions: List[Element]): Unit = {
      val name = element match {
        case Sound.SampleInText(s, _) => s
        case Sound.NoteInText(n, _) => n
        case _ => "unknown"
      }
      val extNames = extensions.map {
        case Sound.SampleInText(s, _) => s
        case Sound.NoteInText(n, _) => n
        case Effect.Gain(v) => s"gain($v)"
        case Effect.Room(v) => s"room($v)"
        case _ => "unknown"
      }
      playedEvents = playedEvents :+ PlayedEvent(name, simulatedNow, durationMs, extNames)
    }
  }

  // Setup in una riga
  def setupPlayer(base: Pattern, extensions: List[Pattern] = Nil, cps: Double = 0.5): TestableAudioPlayer = {
    val player = new TestableAudioPlayer(cps)
    SchedulerImpl.updateTimeline(List(Stream(base, extensions)))
    player
  }

  // Simula un intero ciclo e restituisce SOLO gli eventi di quel ciclo
  def playCycle(player: TestableAudioPlayer, cycleIndex: Int, cps: Double = 0.5): List[PlayedEvent] = {
    val cycleDurationMs = (1000.0 / cps).toLong
    val startMs = cycleIndex * cycleDurationMs
    val endMs = startMs + cycleDurationMs - 1
    for (t <- startMs to endMs) {
      player.tick(t)
    }
    // Estraiamo gli eventi e puliamo la spia
    val events = player.playedEvents
    player.playedEvents = Nil
    events
  }

  //-----------------------------------------------------------------------------
}