package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import soundcode.domain.*

extension (n: Int)
  def \(d: Int): Fraction = Fraction(n.toLong, d.toLong)

extension (n: Long)
  def \(d: Long): Fraction = Fraction(n, d)
// ---------------------------------------------

class SchedulerTest extends AnyFunSuite with Matchers {

  val dummyPos = TextPosition(0, 0)

  // Suoni Base
  def bd = Pattern.Atom(Sound.SampleInText(Sample("bd"), dummyPos))
  def hh = Pattern.Atom(Sound.SampleInText(Sample("hh"), dummyPos))
  def sn = Pattern.Atom(Sound.SampleInText(Sample("sn"), dummyPos))
  def cp = Pattern.Atom(Sound.SampleInText(Sample("cp"), dummyPos))
  def rim = Pattern.Atom(Sound.SampleInText(Sample("rim"), dummyPos))
  def clap = Pattern.Atom(Sound.SampleInText(Sample("clap"), dummyPos))

  // Note
  def c4 = Pattern.Atom(Sound.NoteInText(Note("c4"), dummyPos))
  def f4 = Pattern.Atom(Sound.NoteInText(Note("f4"), dummyPos))
  def g4 = Pattern.Atom(Sound.NoteInText(Note("g4"), dummyPos))
  def cSharp4 = Pattern.Atom(Sound.NoteInText(Note("c#4"), dummyPos))

  // Effetti
  def gain(v: Double) = Pattern.Atom(AudioEffect.Gain(v))
  def room(v: Double) = Pattern.Atom(AudioEffect.Room(v))

  // Combinatori
  def seq[T](p: Pattern[T]*): Pattern[T] = Pattern.Sequence(p.toList)
  def par[T](p: Pattern[T]*): Pattern[T] = Pattern.Parallel(p.toList)
  def alt[T](p: Pattern[T]*): Pattern[T] = Pattern.Alternation(p.toList)
  def ext(base: Pattern[AudioPayload], exts: Pattern[AudioPayload]*): Pattern[AudioPayload] =
    Pattern.WithExtensions(base, exts.toList)

  def resolve(patterns: List[Pattern[AudioPayload]], cycle: Int): List[ExpEvent] =
    val cycleStart = cycle \ 1
    val cycleEnd = (cycle + 1) \ 1

    SchedulerImpl.generateInfiniteTimeline(patterns)
      .dropWhile(_.part.start < cycleStart)
      .takeWhile(_.part.start < cycleEnd)
      .map(_.toExp)
      .toList

  test("bd hh sn hh") {
    val streams = List(seq(bd, hh, sn, hh))

    resolve(streams, 0) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 4),
      ExpEvent("hh", 1 \ 4, 1 \ 2),
      ExpEvent("sn", 1 \ 2, 3 \ 4),
      ExpEvent("hh", 3 \ 4, 1 \ 1)
    )
  }

  test("<bd bd hh bd rim bd hh bd> risolve ogni passo correttamente") {
    val pattern = alt(bd, bd, hh, bd, rim, bd, hh, bd)
    val streams = List(pattern)

    val samples = Vector("bd", "bd", "hh", "bd", "rim", "bd", "hh", "bd")

    for (i <- 0 until 8) {
      val result = resolve(streams, i)

      result should have size 1
      result.head shouldBe ExpEvent(samples(i), i \ 1, (i + 1) \ 1)
    }
  }

  test("note(c f [ g c c# ]).sound(<bd [hh sn]> cp).room(4 5 [4] , <4 5 6>)") {
    val base = seq(c4, f4, seq(g4, c4, cSharp4))
    val extSound = seq(alt(bd, seq(hh, sn)), cp)
    val extRoom = par(seq(room(4), room(5), seq(room(4))), alt(room(4), room(5), room(6)))
    val streams = List(ext(base, extSound, extRoom))

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
    val streams = List(seq(bd, hh, sn, hh, par(hh, seq(sn, alt(bd, hh)))))

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
    val streams = List(seq(alt(bd, seq(hh, sn)), cp))

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
    // Prima era: interpret("sound(\"bd hh sn hh\")\nsound(\"<cp rim bd>\")")
    // Ora passiamo due pattern separati nella lista
    val p1 = seq(bd, hh, sn, hh)
    val p2 = alt(cp, rim, bd)
    val streams = List(p1, p2)

    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 2

    viewData(1).last.part.end shouldBe (3 \ 1)

    viewData(1).map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("cp", 0 \ 1, 1 \ 1),
      ExpEvent("rim", 1 \ 1, 2 \ 1),
      ExpEvent("bd", 2 \ 1, 3 \ 1)
    )
  }

  test("generateBoundedTimelines: Poliritmia tra Base (2) ed Estensione (3) -> MCM = 6 cicli") {
    val streams = List(ext(alt(bd, cp), alt(c4, f4, g4)))
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.part.end shouldBe (6 \ 1)

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
    val streams = List(alt(bd, alt(hh, sn)))
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.part.end shouldBe (4 \ 1)

    trackTimeline.map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 1),
      ExpEvent("hh", 1 \ 1, 2 \ 1),
      ExpEvent("bd", 2 \ 1, 3 \ 1),
      ExpEvent("sn", 3 \ 1, 4 \ 1)
    )
  }

  test("generateBoundedTimelines: Multi-estensioni con lunghezze 1, 2 e 3 -> MCM = 6 cicli") {
    val streams = List(ext(bd, alt(c4, f4), alt(gain(3), gain(4), gain(5))))
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.part.end shouldBe (6 \ 1)

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
    val streams = List(ext(alt(bd, cp), seq(c4, f4, g4)))
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.part.end shouldBe (2 \ 1)

    trackTimeline.map(_.toExp) should contain theSameElementsInOrderAs List(
      ExpEvent("bd", 0 \ 1, 1 \ 1, List("c4")),
      ExpEvent("cp", 1 \ 1, 2 \ 1, List("c4"))
    )
  }

  test("Stress Test 1: Tre livelli di annidamento <bd <hh <sn cp>>> (MCM = 8)") {
    val streams = List(alt(bd, alt(hh, alt(sn, cp))))
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.part.end shouldBe (8 \ 1)

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
    val streams = List(alt(bd, cp, alt(hh, alt(sn, rim, clap))))
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.part.end shouldBe (18 \ 1)

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
    val streams = List(ext(alt(bd, cp), alt(c4, f4, g4), alt(gain(1), gain(2), gain(3), gain(4), gain(5))))
    val viewData = SchedulerImpl.generateBoundedTimelines(streams)

    viewData should have size 1
    val trackTimeline = viewData.head

    trackTimeline.last.part.end shouldBe (30 \ 1)
    trackTimeline should have size 30

    val events = trackTimeline.map(_.toExp).toVector

    events(0) shouldBe ExpEvent("bd", 0 \ 1, 1 \ 1, List("c4", "1.0"))
    events(14) shouldBe ExpEvent("bd", 14 \ 1, 15 \ 1, List("g4", "5.0"))
    events(29) shouldBe ExpEvent("cp", 29 \ 1, 30 \ 1, List("g4", "5.0"))
  }
}

// Classe ExpEvent invariata, accetta Fraction
case class ExpEvent(element: String, start: Fraction, end: Fraction, extensions: List[String] = Nil)

extension (e: ScheduledEvent[AudioPayload])
  def toExp: ExpEvent =
    def extractName(payload: AudioPayload): String = payload match
      case Sound.SampleInText(s, _) => s.value
      case Sound.NoteInText(n, _) => n.value
      case AudioEffect.Gain(v) => v.toString
      case AudioEffect.Room(v) => v.toString
      case AudioEffect.Pan(v) => v.toString
      case _ => "unknown"

    val name = extractName(e.value)
    val exts = e.appliedExtensions.map(extractName)

    ExpEvent(name, e.part.start, e.part.end, exts)