package soundcode.ui.visualizer

import scalafx.scene.Node

trait AnimatedView:
  def root: Node
  def play(): Unit
  def stop(): Unit

final case class VisualizerConfig(
    verticalPadding: Double = 5.0,
    horizontalPadding: Double = 8.0
)
