package soundcode.engine

import soundcode.domain.*

object PatternResolver:

  def query(pattern: Pattern[AudioPayload], timeWindow: Interval): List[ScheduledEvent[AudioPayload]] =
    if timeWindow.start >= timeWindow.end then return Nil
    pattern match
      case Pattern.Atom(value) => List(ScheduledEvent(whole = timeWindow, part = timeWindow, value))
      case Pattern.Parallel(layers) => layers.flatMap(layer => query(layer, timeWindow))
      case Pattern.Alternation(elements) => resolveAlternation(elements, timeWindow)
      case Pattern.Sequence(elements) => resolveSequence(elements, timeWindow)
      case Pattern.WithExtensions(base, extensions) => resolveExtensions(base, extensions, timeWindow)
      case Pattern.TimeWarp(modifier, innerPattern) =>
        // Per ora disattivato
        query(innerPattern, timeWindow)

  private def resolveAlternation(elements: List[Pattern[AudioPayload]], timeWindow: Interval): List[ScheduledEvent[AudioPayload]] =
    if elements.isEmpty then return Nil

    val cycle = timeWindow.start.toDouble.floor.toLong
    val activeIndex = (cycle.abs % elements.size).toInt
  
    // Calcoliamo lo shift temporale in un'unica riga
    val timeOffset = Fraction(cycle - (cycle / elements.size), 1L)
  
    // Pipeline funzionale: Sposta indietro -> Interroga -> Sposta avanti
    query(elements(activeIndex), shiftInterval(timeWindow, backwardsBy = timeOffset))
      .map(event => shiftEvent(event, forwardsBy = timeOffset))


  private def resolveSequence(elements: List[Pattern[AudioPayload]], timeWindow: Interval): List[ScheduledEvent[AudioPayload]] =
    if elements.isEmpty then return Nil
    val n = elements.size
    val step = Fraction(1, n)
    val startCycle = timeWindow.start.toDouble.floor.toLong
    val endCycle = timeWindow.end.toDouble.ceil.toLong

    val events = for cycle <- startCycle until endCycle
      cycleStart = Fraction(cycle, 1L)

      (element, index) <- elements.zipWithIndex
      slotStart = cycleStart + (step * Fraction(index.toLong, 1L))
      slot = Interval(slotStart, slotStart + step)

      overlap <- slot.intersect(timeWindow).toList

      // Zoom In -> Query -> Zoom Out -> Clip
      zoomedInWindow = zoomInTime(overlap, slotStart, n, cycleStart)
      childEvent <- query(element, zoomedInWindow)

      zoomedOutEvent = zoomOutEvent(childEvent, slotStart, n, cycleStart)
      finalEvent <- clipEventToWindow(zoomedOutEvent, overlap).toList
    yield finalEvent

    events.toList


  private def resolveExtensions(base: Pattern[AudioPayload], extensions: List[Pattern[AudioPayload]], timeWindow: Interval): List[ScheduledEvent[AudioPayload]] =
    query(base, timeWindow).map { baseEvent =>
      val activeExts = extensions
        .flatMap(ext => query(ext, baseEvent.part))
        .filter(e => e.part.start <= baseEvent.part.start && e.part.end > baseEvent.part.start)
        .map(_.value)
      baseEvent.copy(appliedExtensions = baseEvent.appliedExtensions ++ activeExts)
    }

  // HELPER MATEMATICI PER LA MANIPOLAZIONE DEL TEMPO (GEOMETRIA)

  private def shiftInterval(interval: Interval, backwardsBy: Fraction): Interval =
    Interval(interval.start - backwardsBy, interval.end - backwardsBy)

  private def shiftEvent(event: ScheduledEvent[AudioPayload], forwardsBy: Fraction): ScheduledEvent[AudioPayload] =
    event.copy(
      whole = Interval(event.whole.start + forwardsBy, event.whole.end + forwardsBy),
      part = Interval(event.part.start + forwardsBy, event.part.end + forwardsBy)
    )

  // Trasforma il tempo globale nello spazio di tempo "ingrandito" del figlio
  private def zoomInTime(globalWindow: Interval, slotStart: Fraction, zoomFactor: Int, cycleStart: Fraction): Interval =
    def applyZoom(time: Fraction): Fraction = (time - slotStart) * Fraction(zoomFactor.toLong, 1L) + cycleStart
    Interval(applyZoom(globalWindow.start), applyZoom(globalWindow.end))

  // Trasforma il tempo locale del figlio riportandolo alla dimensione originale
  private def zoomOutEvent(event: ScheduledEvent[AudioPayload], slotStart: Fraction, zoomFactor: Int, cycleStart: Fraction): ScheduledEvent[AudioPayload] =
    def applyUnzoom(time: Fraction): Fraction = ((time - cycleStart) / Fraction(zoomFactor.toLong, 1L)) + slotStart

    event.copy(
      whole = Interval(applyUnzoom(event.whole.start), applyUnzoom(event.whole.end)),
      part = Interval(applyUnzoom(event.part.start), applyUnzoom(event.part.end))
    )

  private def clipEventToWindow(event: ScheduledEvent[AudioPayload], window: Interval): Option[ScheduledEvent[AudioPayload]] =
    event.part.intersect(window).map { validPart =>
      event.copy(part = validPart)
    }