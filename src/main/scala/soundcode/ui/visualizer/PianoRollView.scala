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
import scalafx.scene.canvas.Canvas
import scalafx.animation.AnimationTimer
import scalafx.scene.paint.Color
import scalafx.scene.canvas.GraphicsContext
import scalafx.application.Platform

final class PianoRollView extends CanvasAnimatedView:
  private val minPitch = notes.map(_.pitch).min
  private val maxPitch = notes.map(_.pitch).max

  private val pitchHeight =
    (canvasHeight - (config.verticalPadding * 2)) / (maxPitch - minPitch + 1)

  override protected def draw(
      gc: GraphicsContext,
      currentBeat: Double,
      w: Double,
      h: Double
  ): Unit =
    val playheadX = w * 0.5

    drawPlayhead(gc, playheadX, h)
    drawNotes(gc, currentBeat, playheadX)

  private def drawPlayhead(gc: GraphicsContext, x: Double, h: Double): Unit =
    gc.stroke = Color.White
    gc.lineWidth = 2
    gc.strokeLine(x, config.verticalPadding, x, h - config.verticalPadding)

  private def drawNotes(
      gc: GraphicsContext,
      currentBeat: Double,
      playheadX: Double
  ): Unit =
    val lenght = loopLength
    val currentLoop = Math.floor(currentBeat / lenght).toInt

    val repeatedNotes =
      for
        loop <- (currentLoop - 1).max(0) to currentLoop + 2
        note <- notes
      yield note.copy(start = note.start + loop * lenght)

    repeatedNotes.foreach { note =>
      val x = playheadX + (note.start - currentBeat) * pixelsPerBeat
      val y = (maxPitch - note.pitch) * pitchHeight + config.verticalPadding
      val width = note.duration * pixelsPerBeat
      val height = pitchHeight

      val isActive =
        currentBeat >= note.start && currentBeat < note.start + note.duration

      if isActive then
        gc.stroke = Color.White
        gc.lineWidth = 2
        gc.strokeRect(x, y, width, height)
      else
        gc.fill = Color.rgb(255, 255, 255)
        gc.fillRect(x, y, width, height)
    }
