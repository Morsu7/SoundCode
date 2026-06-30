package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.*
import soundcode.interpreter.interpret
import soundcode.engine.Resolvable.given

// --- SYNTAX SUGAR PER LE FRAZIONI ---
extension (n: Int)
  def \(d: Int): Fraction = Fraction(n.toLong, d.toLong)

extension (n: Long)
  def \(d: Long): Fraction = Fraction(n, d)
// ------------------------------------

class SchedulerTest extends AnyFunSuite with Matchers {

  // Limiti calcolati dinamicamente usando la nuova sintassi
  def resolve(tracks: List[Track], cycle: Int): List[ExpEvent] =
    val cycleStart = cycle \ 1
    val cycleEnd = (cycle + 1) \ 1

    SchedulerImpl.generateInfiniteTimeline(tracks)
      .dropWhile(_.startTime < cycleStart)
      .takeWhile(_.startTime < cycleEnd)
      .map(_.toExp)
      .toList

  test("bd hh sn hh") {
    val streams = interpret("sound(\"bd hh sn hh\")")

    resolve(streams, 0) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 4),
      ExpEvent("hh", 1 \ 4, 1 \ 2),
      ExpEvent("sn", 1 \ 2, 3 \ 4),
      ExpEvent("hh", 3 \ 4, 1 \ 1)
    )
  }

  test("<bd bd hh bd rim bd hh bd>") {
    val streams = interpret("sound(\"<bd bd hh bd rim bd hh bd>\")")
    val outcomes = List("bd", "bd", "hh", "bd", "rim", "bd", "hh", "bd")

    for (cycleIndex <- 0 until 8) {
      resolve(streams, cycleIndex) should contain theSameElementsInOrderAs List(
        ExpEvent(outcomes(cycleIndex), cycleIndex \ 1, (cycleIndex + 1) \ 1)
      )
    }
  }

  test("note(c f [ g c c# ]).sound(<bd [hh sn]> cp).room(4 5 [4] , <4 5 6>)") {
    val streams = interpret("note(\"c f [ g c c# ]\").sound(\"<bd [hh sn]> cp\").room(\"4 5 [4] , <4 5 6>\")")

    resolve(streams, 0) should contain theSameElementsInOrderAs List(
      ExpEvent("c4", 0 \ 1, 1 \ 3, List("bd", "4.0", "4.0")),
      ExpEvent("f4", 1 \ 3, 2 \ 3, List("bd", "5.0", "4.0")),
      ExpEvent("g4", 2 \ 3, 7 \ 9, List("cp", "4.0", "4.0")),
      ExpEvent("c4", 7 \ 9, 8 \ 9, List("cp", "4.0", "4.0")),
      ExpEvent("c#4", 8 \ 9, 1 \ 1, List("cp", "4.0", "4.0"))
    )

    resolve(streams, 1) should contain theSameElementsInOrderAs List(
      ExpEvent("c4", 1 \ 1, 4 \ 3, List("hh", "4.0", "5.0")),
      ExpEvent("f4", 4 \ 3, 5 \ 3, List("sn", "5.0", "5.0")),
      ExpEvent("g4", 5 \ 3, 16 \ 9, List("cp", "4.0", "5.0")),
      ExpEvent("c4", 16 \ 9, 17 \ 9, List("cp", "4.0", "5.0")),
      ExpEvent("c#4", 17 \ 9, 2 \ 1, List("cp", "4.0", "5.0"))
    )
  }

  test("bd hh sn hh [hh , sn < bd hh > ]") {
    val streams = interpret("sound(\"bd hh sn hh [hh , sn < bd hh > ]\")")

    resolve(streams, 0) should contain allOf(
      ExpEvent("bd", 0 \ 1, 1 \ 5),
      ExpEvent("hh", 1 \ 5, 2 \ 5),
      ExpEvent("sn", 2 \ 5, 3 \ 5),
      ExpEvent("hh", 3 \ 5, 4 \ 5),
      ExpEvent("hh", 4 \ 5, 1 \ 1),
      ExpEvent("sn", 4 \ 5, 9 \ 10),
      ExpEvent("bd", 9 \ 10, 1 \ 1)
    )

    resolve(streams, 1) should contain allOf(
      ExpEvent("bd", 1 \ 1, 6 \ 5),
      ExpEvent("hh", 6 \ 5, 7 \ 5),
      ExpEvent("sn", 7 \ 5, 8 \ 5),
      ExpEvent("hh", 8 \ 5, 9 \ 5),
      ExpEvent("hh", 9 \ 5, 2 \ 1),
      ExpEvent("sn", 9 \ 5, 19 \ 10),
      ExpEvent("hh", 19 \ 10, 2 \ 1)
    )
  }

  test("<bd [hh sn]> cp") {
    val streams = interpret("sound(\"<bd [hh sn]> cp\")")

    resolve(streams, 0) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 2),
      ExpEvent("cp", 1 \ 2, 1 \ 1)
    )

    resolve(streams, 1) should contain theSameElementsInOrderAs List(
      ExpEvent("hh", 1 \ 1, 5 \ 4),
      ExpEvent("sn", 5 \ 4, 3 \ 2),
      ExpEvent("cp", 3 \ 2, 2 \ 1)
    )
  }

  test("generateBoundedTimelines rispetta le lunghezze dei pattern") {
    val streams = interpret("sound(\"bd hh sn hh\")\nsound(\"<cp rim bd>\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 2

    viewData(1).last.endTime shouldBe (3 \ 1)

    viewData(1).map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("cp", 0 \ 1, 1 \ 1),
      ExpEvent("rim", 1 \ 1, 2 \ 1),
      ExpEvent("bd", 2 \ 1, 3 \ 1)
    )
  }

  test("generateBoundedTimelines: Poliritmia tra Base (2) ed Estensione (3) -> MCM = 6 cicli") {
    val streams = interpret("sound(\"<bd cp>\").note(\"<c f g>\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.endTime shouldBe (6 \ 1)

    trackTimeline.map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 1, List("c4")),
      ExpEvent("cp", 1 \ 1, 2 \ 1, List("f4")),
      ExpEvent("bd", 2 \ 1, 3 \ 1, List("g4")),
      ExpEvent("cp", 3 \ 1, 4 \ 1, List("c4")),
      ExpEvent("bd", 4 \ 1, 5 \ 1, List("f4")),
      ExpEvent("cp", 5 \ 1, 6 \ 1, List("g4"))
    )
  }

  test("generateBoundedTimelines: Alternanze annidate <bd <hh sn>> durano 4 cicli") {
    val streams = interpret("sound(\"<bd <hh sn>>\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.endTime shouldBe (4 \ 1)

    trackTimeline.map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 1),
      ExpEvent("hh", 1 \ 1, 2 \ 1),
      ExpEvent("bd", 2 \ 1, 3 \ 1),
      ExpEvent("sn", 3 \ 1, 4 \ 1)
    )
  }

  test("generateBoundedTimelines: Multi-estensioni con lunghezze 1, 2 e 3 -> MCM = 6 cicli") {
    val streams = interpret("sound(\"bd\").note(\"<c f>\").gain(\"<3 4 5>\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.endTime shouldBe (6 \ 1)

    trackTimeline.map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 1, List("c4", "3.0")),
      ExpEvent("bd", 1 \ 1, 2 \ 1, List("f4", "4.0")),
      ExpEvent("bd", 2 \ 1, 3 \ 1, List("c4", "5.0")),
      ExpEvent("bd", 3 \ 1, 4 \ 1, List("f4", "3.0")),
      ExpEvent("bd", 4 \ 1, 5 \ 1, List("c4", "4.0")),
      ExpEvent("bd", 5 \ 1, 6 \ 1, List("f4", "5.0"))
    )
  }

  test("generateBoundedTimelines: Le estensioni sub-ciclo non alterano il loop totale") {
    val streams = interpret("sound(\"<bd cp>\").note(\"c f g\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.endTime shouldBe (2 \ 1)

    trackTimeline.map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 1, List("c4")),
      ExpEvent("cp", 1 \ 1, 2 \ 1, List("c4"))
    )
  }

  test("Stress Test 1: Tre livelli di annidamento <bd <hh <sn cp>>> (MCM = 8)") {
    val streams = interpret("sound(\"<bd <hh <sn cp>>>\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.endTime shouldBe (8 \ 1)

    trackTimeline.map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 1),
      ExpEvent("hh", 1 \ 1, 2 \ 1),
      ExpEvent("bd", 2 \ 1, 3 \ 1),
      ExpEvent("sn", 3 \ 1, 4 \ 1),
      ExpEvent("bd", 4 \ 1, 5 \ 1),
      ExpEvent("hh", 5 \ 1, 6 \ 1),
      ExpEvent("bd", 6 \ 1, 7 \ 1),
      ExpEvent("cp", 7 \ 1, 8 \ 1)
    )
  }

  test("Stress Test 2: Annidamento asimmetrico estremo <bd cp <hh <sn rim clap>>> (MCM = 18)") {
    val streams = interpret("sound(\"<bd cp <hh <sn rim clap>>>\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.endTime shouldBe (18 \ 1)

    val events = trackTimeline.map(_.toExp).toVector

    events(0).element shouldBe "bd"
    events(1).element shouldBe "cp"
    events(2).element shouldBe "hh"
    events(3).element shouldBe "bd"
    events(4).element shouldBe "cp"
    events(5).element shouldBe "sn"
    events(17).element shouldBe "clap"
  }

  test("Stress Test 3: Scontro di numeri primi tra base ed estensioni (2, 3, 5 -> MCM = 30)") {
    val streams = interpret("sound(\"<bd cp>\").note(\"<c f g>\").gain(\"<1 2 3 4 5>\")")
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.endTime shouldBe (30 \ 1)
    trackTimeline should have size 30

    val events = trackTimeline.map(_.toExp).toVector

    events(0) shouldBe ExpEvent("bd", 0 \ 1, 1 \ 1, List("c4", "1.0"))
    events(14) shouldBe ExpEvent("bd", 14 \ 1, 15 \ 1, List("g4", "5.0"))
    events(29) shouldBe ExpEvent("cp", 29 \ 1, 30 \ 1, List("g4", "5.0"))
  }
}

// Classe ExpEvent invariata, accetta Fraction
case class ExpEvent(element: String, start: Fraction, end: Fraction, extensions: List[String] = Nil)

extension (e: ScheduledEvent)
  def toExp: ExpEvent =
    val exts = e.appliedExtensions.map {
      case Sound.NoteInText(n, _) => n.value
      case Sound.SampleInText(s, _) => s.value
      case AudioEffect.Gain(v) => v.toString
      case AudioEffect.Room(v) => v.toString
      case _ => "unknown"
    }
    val name = e.element match {
      case Sound.SampleInText(s, _) => s.value
      case Sound.NoteInText(n, _) => n.value
      case _ => "unknown"
    }
    ExpEvent(name, e.startTime, e.endTime, exts)