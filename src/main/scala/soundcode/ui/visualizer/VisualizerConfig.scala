package soundcode.ui.visualizer

final case class VisualizerConfig(
    verticalPadding: Double = 5.0,
    horizontalPadding: Double = 8.0
)

object VisualizerConfig:
  val default: VisualizerConfig = VisualizerConfig()
