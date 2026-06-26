package soundcode.mvu

object Update:
  def update(model: AppModel, msg: Msg): AppModel =
    msg match
      case Msg.CodeUpdateRequested(code) =>
        model.copy(code = code)

      case Msg.PlaybackTick(currentBeat) =>
        model.copy(currentBeat = currentBeat, isPlaying = true)
