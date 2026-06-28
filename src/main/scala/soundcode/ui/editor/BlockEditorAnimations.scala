package soundcode.ui.editor

import scalafx.util.Duration
import scalafx.scene.Node
import scalafx.animation.ParallelTransition
import scalafx.animation.FadeTransition

private[editor] object BlockEditorAnimations:
  private val DurationMs = Duration(120)

  def fadeIn(target: Node): Unit =
    target.opacity = 0.0

    val fade = new FadeTransition(DurationMs, target)
    fade.fromValue = 0.0
    fade.toValue = 1.0

    val transition = new ParallelTransition()
    transition.children = Seq(fade)
    transition.play()

  def fadeOut(target: Node)(onFinishedAction: => Unit): Unit =
    val fade = new FadeTransition(DurationMs, target)
    fade.fromValue = target.opacity.value
    fade.toValue = 0.0

    val transition = new ParallelTransition()
    transition.children = Seq(fade)
    transition.onFinished = _ => onFinishedAction
    transition.play()
