package soundcode.mvu

import soundcode.mvu.Cmd.*

object Update:
  def update(model: AppModel, msg: Msg): (AppModel, Cmd) =
    msg match
      case Msg.CodeUpdateRequested(code) =>
        (
          model.copy(code = code),
          NoOp
        )

      case Msg.PlaybackTick(currentBeat) =>
        (
          //model.copy(currentBeat = currentBeat, isPlaying = true),
          model,
          NoOp
        )
