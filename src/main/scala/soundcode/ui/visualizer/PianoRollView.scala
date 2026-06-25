package soundcode.ui.visualizer

import scalafx.animation.{KeyFrame, PauseTransition, Timeline}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Node
import scalafx.scene.control.Label
import scalafx.scene.layout.{HBox, VBox}
import scalafx.util.Duration
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.GridPane
import soundcode.ui.visualizer.AnimatedView

final class PianoRollView extends AnimatedView:
  private val view = new VBox:
    spacing = 4
    padding = Insets(8)
    children = Seq(
      new Label("Piano Roll")
    )

  override val root: Node = view

  override def play(): Unit = println("PianoRollView: play() called")
  override def stop(): Unit = println("PianoRollView: stop() called")
