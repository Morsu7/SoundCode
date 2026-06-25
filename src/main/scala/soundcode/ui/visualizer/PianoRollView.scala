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

private case class FakeNote(
    pitch: Int,
    start: Double,
    duration: Double
)

private val notes = Seq(
  FakeNote(60, 0.0, 0.75),
  FakeNote(64, 0.5, 0.5),
  FakeNote(67, 1.0, 1.0),
  FakeNote(72, 2.0, 0.5),
  FakeNote(69, 2.5, 0.75),
  FakeNote(65, 3.25, 0.5),
  FakeNote(60, 4.0, 1.0),
  FakeNote(67, 5.0, 0.75),
  FakeNote(72, 6.0, 1.0)
)

final class PianoRollView(
    config: VisualizerConfig = VisualizerConfig()
) extends AnimatedView:
  // FIXME: must get from a global config
  private val bpm = 120.0

  private val pixelsPerBeat = 96.0
  private val minPitch = notes.map(_.pitch).min
  private val maxPitch = notes.map(_.pitch).max
  private val canvasHeight = 120.0

  private val pitchHeight =
    (canvasHeight - (config.verticalPadding * 2)) / (maxPitch - minPitch + 1)

  private val canvas = new Canvas(0, canvasHeight)

  private def loopLength: Double =
    if notes.isEmpty then 1.0
    else notes.map(note => note.start + note.duration).max

  private val view = new VBox:
    spacing = 4
    padding = Insets(config.horizontalPadding)
    children = Seq(
      new Label("Piano Roll"),
      canvas
    )

  canvas.width <== view.width - config.horizontalPadding * 2

  override val root: Node = view

  private var running = false
  private var startNano: Long = 0L
  private var beatOffset = 0.0
  private var lastBeat = 0.0

  canvas.width.onChange {
    if canvas.width.value > 0 then draw(lastBeat)
  }

  Platform.runLater {
    draw(0.0)
  }

  private val timer = AnimationTimer { now =>
    if running then
      if startNano == 0L then startNano = now

      val elapsedSeconds = (now - startNano) / 1e9
      val currentBeat = beatOffset + elapsedSeconds * bpm / 60.0

      draw(currentBeat)
  }

  private def draw(currentBeat: Double): Unit =
    lastBeat = currentBeat

    val gc = canvas.graphicsContext2D
    val w = canvas.width.value
    val h = canvas.height.value

    val playheadX = w * 0.5

    gc.fill = Color.rgb(31, 31, 36)
    gc.fillRect(0, 0, w, h)

    drawPlayhead(gc, playheadX, h)
    drawNote(gc, currentBeat, playheadX)

  private def drawPlayhead(gc: GraphicsContext, x: Double, h: Double): Unit =
    gc.stroke = Color.White
    gc.lineWidth = 2
    gc.strokeLine(x, config.verticalPadding, x, h - config.verticalPadding)

  private def drawNote(
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

  override def play(): Unit =
    running = true
    startNano = 0L
    timer.start()

  override def stop(): Unit =
    running = false
    timer.stop()
