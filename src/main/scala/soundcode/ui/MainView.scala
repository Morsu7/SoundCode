package soundcode.ui

import scalafx.scene.layout.BorderPane
import scalafx.geometry.Insets
import scalafx.scene.control.ToolBar
import scalafx.scene.control.Button
import scalafx.scene.control.Label

class MainView:
  private val editorView =
    BlockEditorView(
      initialCode = """
        |note("c4 a4").sound("piano")
        |sound("hb hd hh")
      """.stripMargin.trim,
      onCodeChanged = code => {} // TODO: handle code changes
    )

  val root: BorderPane = new BorderPane:
    padding = Insets(10)
    center = editorView.root
    top = toolbar

  private def toolbar: ToolBar =
    new ToolBar:
      content = Seq(
        new Button("Play"):
          onAction = _ => println("Play clicked")
        ,
        new Button("Stop"):
          onAction = _ => println("Stop clicked")
        ,
        new Button("Update"):
          onAction = _ => println("Update clicked")
      )
