package soundcode.engine

import org.scalatest.funsuite.AnyFunSuite
import soundcode.domain.Note

class TimeSchedulerSuite extends AnyFunSuite {

  test("Calcola la somma totale delle durate delle note") {
    // 1. Organizziamo: creiamo una traccia con due note da 2 battiti l'una
    val track = List(Note("C", 2), Note("G", 2))

    // 2. Agiamo: chiediamo allo scheduler di calcolare il tempo
    val totalTime = TimeScheduler.calculateTotalTime(track)

    // 3. Verifichiamo: ci aspettiamo che 2 + 2 faccia 4.
    // Questo test FALLIRÀ perché il metodo restituisce 0.
    assert(totalTime == 4)
  }
}