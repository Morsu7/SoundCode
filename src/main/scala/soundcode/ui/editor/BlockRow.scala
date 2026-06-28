package soundcode.ui.editor

import org.fxmisc.richtext.InlineCssTextArea
import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.control.Label
import soundcode.ui.visualizer.AnimatedView

private[editor] final case class BlockRow(
    editor: InlineCssTextArea,
    codeRow: HBox,
    node: VBox,
    lineNumberLabel: Label,
    visualizer: Option[AnimatedView]
):
  def text: String = editor.getText

  def withVisualizer(next: Option[AnimatedView]): BlockRow =
    node.children = Seq(codeRow) ++ next.map(_.root).toSeq
    copy(visualizer = next)
