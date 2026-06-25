package soundcode.ui.visualizer

import scalafx.scene.layout.VBox
import scalafx.scene.control.Label
import scalafx.scene.Node
import scalafx.geometry.Insets

class OscilloscopeView extends AnimatedView:
  private val view = new VBox:
    spacing = 4
    padding = Insets(8)
    children = Seq(
      new Label("Oscilloscope")
    )

  override val root: Node = view

  override def play(): Unit = println("OscilloscopeView: play() called")
  override def stop(): Unit = println("OscilloscopeView: stop() called")
