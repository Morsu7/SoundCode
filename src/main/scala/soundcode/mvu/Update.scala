package soundcode.mvu

import soundcode.mvu.Cmd.*

object Update:
  def update(model: AppModel, msg: Msg): (AppModel, Cmd) =
    msg match
      case Msg.CodeUpdateRequested(code) =>
        ( 
          model.copy(code = code),
          Cmd.ParseAndInterpret(code)
        )
      
      case Msg.CodeParsed(streams, errors) =>
        println(s"Code parsed with errors: $errors")
        println(s"Parsed streams: $streams")
        (
          model.copy(streams = streams),
          NoOp
        )

      case Msg.PlaybackTick(currentBeat) =>
        (
          //model.copy(currentBeat = currentBeat, isPlaying = true),
          model,
          NoOp
        )

      case _ =>
        (
          model,
          NoOp
        )
