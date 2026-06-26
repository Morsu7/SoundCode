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

class MainView(
    dispatch: Msg => Unit
):
  private val editorView = new BlockEditorView

  def render(model: AppModel): Unit =
    editorView.renderCode(model.code)

  val root: BorderPane = new BorderPane:
    padding = Insets(10)
    center = editorView.root
    top = toolbar

  private def toolbar: ToolBar =
    new ToolBar:
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
