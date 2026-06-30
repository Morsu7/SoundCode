package soundcode.ui.visualizer

import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.layout.VBox
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Label
import scalafx.geometry.Insets
import scalafx.scene.paint.Color
import scalafx.animation.AnimationTimer
import scalafx.application.Platform
import scalafx.scene.layout.Pane

private case class FakeNote(
    pitch: Int,
    start: Double,
    duration: Double
)

private val notes: Seq[FakeNote] = Seq(
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

trait AnimatedView:
  def root: Node
  def play(): Unit
  def stop(): Unit

abstract class CanvasAnimatedView extends AnimatedView:
  // TODO: just mockup bpm for now, fetch from global state later
  protected val bpm = 120.0
  protected val pixelsPerBeat = 96.0

  protected val config = VisualizerConfig.default

  protected val canvasHeight = 120.0
  protected val canvas = new Canvas(0, canvasHeight)

  private val canvasPane = new Pane:
    minWidth = 0
    prefWidth = 0
    maxWidth = Double.MaxValue
    minHeight = canvasHeight
    prefHeight = canvasHeight
    maxHeight = canvasHeight
    children = canvas

  canvas.managed = false
  canvas.height = canvasHeight

  private val view = new VBox:
    spacing = 4
    padding = Insets(config.horizontalPadding)
    minWidth = 0
    prefWidth = 0
    maxWidth = Double.MaxValue
    children = canvasPane

  canvasPane.prefWidthProperty().bind(view.widthProperty())
  canvas.widthProperty().bind(canvasPane.widthProperty())

  canvas.width.onChange {
    redraw(lastBeat)
  }

  override val root: Node = view

  private var running = false
  private var startNano: Long = 0L
  private var lastBeat = 0.0

  protected def loopLength: Double =
    if notes.isEmpty then 1.0
    else notes.map(note => note.start + note.duration).max

  protected def clear(gc: GraphicsContext, w: Double, h: Double): Unit =
    gc.fill = Color.web("#1f1f24")
    gc.fillRect(0, 0, w, h)

  protected def draw(
      gc: GraphicsContext,
      currentBeat: Double,
      w: Double,
      h: Double
  ): Unit

  private def redraw(currentBeat: Double): Unit =
    lastBeat = currentBeat

    val gc = canvas.graphicsContext2D
    val w = canvas.width.value
    val h = canvas.height.value

    if w <= 0 || h <= 0 then return

    clear(gc, w, h)
    draw(gc, currentBeat, w, h)

  Platform.runLater {
    redraw(0.0)
  }

  private val timer = AnimationTimer { now =>
    if running then
      if startNano == 0L then startNano = now

      val elapsedSeconds = (now - startNano) / 1e9
      val currentBeat = elapsedSeconds * bpm / 60.0

      redraw(currentBeat)
  }

  override def play(): Unit =
    running = true
    startNano = 0L
    timer.start()

  override def stop(): Unit =
    running = false
    timer.stop()
