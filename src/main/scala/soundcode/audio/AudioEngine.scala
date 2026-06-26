package soundcode.audio

trait AudioEngine {
  def triggerSound(soundOrNote: String): Unit
}
