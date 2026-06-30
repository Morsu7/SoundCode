package soundcode.ui.editor

import javafx.scene.Node
import javafx.scene.text.{Text, TextFlow}
import javafx.util.Duration
import org.fxmisc.flowless.VirtualizedScrollPane
import org.fxmisc.richtext.{GenericStyledArea, LineNumberFactory}
import org.fxmisc.richtext.model.{SegmentOps, StyledSegment, TextOps}
import org.reactfx.util.{Either as RichEither}
import scalafx.Includes.jfxNode2sfx
import scalafx.application.Platform
import scalafx.scene.layout.StackPane
import soundcode.ui.visualizer.AnimatedView

import javafx.scene.text.FontSmoothingType
import java.util.function.{BiConsumer, Function}
import scala.jdk.CollectionConverters.*
import java.util.Optional
import soundcode.mvu.AppModel

final class BlockEditorView:
  private type Segment = RichEither[String, EmbeddedVisualizerSegment]

  private val textOps = SegmentOps.styledTextOps[String]()
  private val segmentOps: TextOps[Segment, String] =
    TextOps.eitherL[String, EmbeddedVisualizerSegment, String](
      textOps,
      EmbeddedVisualizerSegmentOps,
      (_, _) => Optional.empty()
    )

  private var firstRender = true
  private var visualizers = Vector.empty[AnimatedView]
  private var highlightScheduled = false
  private var replacingCode = false

  private def applyEditorChrome(): Unit =
    Platform.runLater {
      area.lookupAll(".caret").asScala.foreach {
        _.setStyle("-fx-stroke: #f4f4f5;")
      }

      area.lookupAll(".paragraph-box").asScala.foreach {
        _.setStyle("-fx-background-color: #1f1f24;")
      }
    }

  private def isVisualizerParagraph(paragraphIndex: Int): Boolean =
    area
      .getParagraph(paragraphIndex)
      .getSegments
      .asScala
      .exists(_.isRight)

  private def visibleLineNumber(paragraphIndex: Int): Int =
    (0 to paragraphIndex)
      .count(index => !isVisualizerParagraph(index))

  private val area: GenericStyledArea[Unit, Segment, String] =
    new GenericStyledArea[Unit, Segment, String](
      (),
      new BiConsumer[TextFlow, Unit]:
        override def accept(t: TextFlow, u: Unit): Unit = ()
      ,
      "",
      segmentOps,
      new Function[StyledSegment[Segment, String], Node]:
        override def apply(styled: StyledSegment[Segment, String]): Node =
          val segment = styled.getSegment

          if segment.isLeft then
            val text = new Text(segment.getLeft)
            text.setFontSmoothingType(FontSmoothingType.GRAY)
            text.setStyle(styled.getStyle)
            text
          else
            segment.getRight match
              case EmbeddedVisualizerSegment.Empty =>
                new javafx.scene.Group()

              case EmbeddedVisualizerSegment.View(view) =>
                val wrapper = new javafx.scene.layout.VBox()
                wrapper.setFillWidth(true)
                wrapper.setMinWidth(0)
                wrapper.setMaxWidth(Double.MaxValue)
                wrapper
                  .prefWidthProperty()
                  .bind(area.widthProperty().subtract(48))
                wrapper.setStyle("-fx-padding: 4 0 8 0;")

                view.root.delegate match
                  case region: javafx.scene.layout.Region =>
                    region.setMinWidth(0)
                    region.setMaxWidth(Double.MaxValue)
                    region.prefWidthProperty().bind(wrapper.widthProperty())
                  case _ =>

                wrapper.getChildren.add(view.root.delegate)
                wrapper
    )

  area.setWrapText(true)
  area.setParagraphGraphicFactory(
    new java.util.function.IntFunction[Node]:
      override def apply(line: Int): Node =
        val label = new javafx.scene.control.Label()

        label.setText(
          if isVisualizerParagraph(line) then ""
          else visibleLineNumber(line).toString
        )

        label.setStyle(
          """
            |-fx-background-color: #1f1f24;
            |-fx-text-fill: #8b949e;
            |-fx-font-family: 'Cascadia Code', 'JetBrains Mono', 'Consolas';
            |-fx-font-size: 13px;
            |-fx-padding: 0 8 0 4;
            |-fx-alignment: center-right;
            |""".stripMargin
        )

        label.setMinWidth(24)
        label.setPrefWidth(24)

        label
  )
  area.setStyle(
    """
      |-fx-font-family: 'Cascadia Code', 'JetBrains Mono', 'Consolas';
      |-fx-font-size: 15px;
      |-fx-background-color: #1f1f24;
      |-fx-control-inner-background: #1f1f24;
      |-fx-text-fill: #f4f4f5;
      |""".stripMargin
  )
  applyEditorChrome()

  AutoPairingSupport.install(area)

  area.textProperty().addListener { (_, _, _) =>
    if !replacingCode then scheduleHighlight()
  }

  val root: StackPane = new StackPane:
    children = Seq(jfxNode2sfx(new VirtualizedScrollPane(area)))

  def render(state: AppModel): Unit =
    if firstRender then
      firstRender = false
      // setCodeWithVisualizers(code)

  def currentCode: String =
    area.getText
      .replace("\r\n", "\n")
      .replace("\r", "\n")
      .linesIterator
      .filterNot(_.trim.isEmpty)
      .mkString("\n")

  def play(): Unit =
    visualizers.foreach(_.play())

  def stop(): Unit =
    visualizers.foreach(_.stop())

  private def setCodeWithVisualizers(code: String): Unit =
    val normalized = code.replace("\r\n", "\n").replace("\r", "\n")

    replacingCode = true

    try
      area.replaceText(normalized)
      SyntaxHighlighter.applyTo(area)

      visualizers = Vector.empty

      visualizerInsertions(normalized).reverse.foreach {
        case (position, view) =>
          visualizers = visualizers :+ view
          area.insertText(position, "\n")
          area.insert(
            position + 1,
            RichEither.right[String, EmbeddedVisualizerSegment](
              EmbeddedVisualizerSegment.View(view)
            ),
            ""
          )
      }
    finally replacingCode = false

  private def visualizerInsertions(code: String): Vector[(Int, AnimatedView)] =
    val lines = code.split("\n", -1).toVector

    var offset = 0

    lines.zipWithIndex.flatMap { case (line, index) =>
      val lineEnd = offset + line.length
      offset = lineEnd + (if index < lines.length - 1 then 1 else 0)

      BlockEditorVisualizers.forLine(line).map(view => lineEnd -> view)
    }

  private def scheduleHighlight(): Unit =
    if !highlightScheduled then
      highlightScheduled = true

      Platform.runLater {
        highlightScheduled = false
        SyntaxHighlighter.applyTo(area)
      }
