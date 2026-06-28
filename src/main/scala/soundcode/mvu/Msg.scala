package soundcode.mvu

import soundcode.domain.{Sound, TextPosition}

enum Msg:
  case CodeUpdateRequested(code: String)
  // possible implementation of a tick message to update the current beat in the piano roll view
  case PlaybackTick(currentBeat: Double)
  case UpdateHighlightText(positions: Set[TextPosition])
  case UpdateTimelines(timelines: List[Seq[Sound]])
