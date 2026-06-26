package soundcode.ui

import soundcode.ui.visualizer.OscilloscopeView
import soundcode.ui.visualizer.PianoRollView

class AnimatedViewSpec extends UITestSupport:
  test("pianoroll and oscilloscope view exposes a root node"):
    onFxThread:
      val oscilloView = new OscilloscopeView()
      val pianoView = new PianoRollView()

      assert(oscilloView.root != null)
      assert(pianoView.root != null)

  test("pianoroll and oscilloscope view can play and stop"):
    onFxThread:
      val oscilloView = new OscilloscopeView()
      val pianoView = new PianoRollView()

      oscilloView.play()
      oscilloView.stop()

      pianoView.play()
      pianoView.stop()

      assert(oscilloView.root != null)
      assert(pianoView.root != null)
