package soundcode.ui.editor

import soundcode.ui.visualizer.AnimatedView
import soundcode.ui.visualizer.PianoRollView
import soundcode.ui.visualizer.OscilloscopeView

private object BlockEditorVisualizers:
  def forLine(line: String): Option[AnimatedView] =
    if line.contains("._pianoroll()") then Some(new PianoRollView())
    else if line.contains("._scope()") then Some(new OscilloscopeView())
    else None
