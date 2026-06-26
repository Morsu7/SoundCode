package soundcode.mvu

enum Msg:
  case CodeUpdateRequested(code: String)
  // possible implementation of a tick message to update the current beat in the piano roll view
  case PlaybackTick(currentBeat: Double)
