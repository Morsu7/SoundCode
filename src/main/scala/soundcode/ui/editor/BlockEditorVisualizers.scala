package soundcode.ui.editor

import soundcode.ui.visualizer.AnimatedView
import soundcode.ui.visualizer.PianoRollView
import soundcode.ui.visualizer.OscilloscopeView

private[editor] object BlockEditorVisualizers:
  def forLine(line: String): Option[AnimatedView] =
    if line.contains("._pianoroll()") then Some(new PianoRollView())
    else if line.contains("._scope()") then Some(new OscilloscopeView())
    else None

  def matches(line: String, visualizer: Option[AnimatedView]): Boolean =
    visualizer match
      case Some(_: PianoRollView) =>
        line.contains("._pianoroll()")
      case Some(_: OscilloscopeView) =>
        line.contains("._scope()")
      case None =>
        !line.contains("._pianoroll()") && !line.contains("._scope()")
      case _ =>
        false
