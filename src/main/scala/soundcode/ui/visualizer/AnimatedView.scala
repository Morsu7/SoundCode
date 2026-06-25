package soundcode.ui.visualizer

import scalafx.scene.Node

trait AnimatedView:
  def root: Node
  def play(): Unit
  def stop(): Unit
