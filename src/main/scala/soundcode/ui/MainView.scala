package soundcode.ui

import scalafx.scene.layout.BorderPane
import scalafx.geometry.Insets
import scalafx.scene.control.ToolBar
import scalafx.scene.control.Button
import scalafx.scene.control.Label

import javafx.event.{ActionEvent, EventHandler}
import soundcode.ui.editor.BlockEditorView
import soundcode.mvu.Msg
import soundcode.mvu.AppModel
import scala.annotation.nowarn

class MainView(
    dispatch: Msg => Unit
):
  private val editorView = new BlockEditorView

  def render(state: AppModel): Unit =
    editorView.render(state)

  val root: BorderPane = new BorderPane:
    padding = Insets(10)
    style = "-fx-background-color: #1f1f24;"
    center = editorView.root
    top = toolbar

  // to remove this nowarn we should update the scalfx version to a version that supports scala 3
  @nowarn("msg=Implicit parameters should be provided with a `using` clause")
  private def toolbar: ToolBar =
    new ToolBar:
      style = "-fx-background-color: #1f1f24; -fx-padding: 6;"
      content = Seq(
        new Button("Play"):
          onAction = _ => editorView.play()
        ,
        new Button("Stop"):
          onAction = _ => editorView.stop()
        ,
        new Button("Update"):
          onAction =
            _ => dispatch(Msg.CodeUpdateRequested(editorView.currentCode))
      )
