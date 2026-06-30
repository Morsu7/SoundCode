package soundcode.ui

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import soundcode.mvu.SoundCodeRuntime
import soundcode.mvu.AppModel
import soundcode.mvu.Update

object SoundCodeFrame extends JFXApp3:
  override def start(): Unit =
    val initialModel = AppModel()

    lazy val mainView: MainView = MainView(runtime.dispatch)

    lazy val runtime: SoundCodeRuntime =
      SoundCodeRuntime(
        initialModel = initialModel,
        render = model => mainView.render(model)
      )
    mainView.render(initialModel)

    stage = new JFXApp3.PrimaryStage:
      title = "SoundCode"
      scene = new Scene(800, 500):
        root = mainView.root

  override def stopApp(): Unit =
    super.stopApp()
