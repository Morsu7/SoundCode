package soundcode.ui

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import soundcode.mvu.SoundCodeRuntime
import soundcode.mvu.AppModel
import soundcode.mvu.Update

object SoundCodeFrame extends JFXApp3:
  override def start(): Unit =
    var mainView: MainView = null
    val initialModel = AppModel()

    val runtime = SoundCodeRuntime(
      initialModel = initialModel,
      render =
        (model: AppModel) => if mainView != null then mainView.render(model)
    )

    mainView = MainView(runtime.dispatch)
    mainView.render(initialModel)

    stage = new JFXApp3.PrimaryStage:
      title = "SoundCode"
      scene = new Scene(800, 500):
        root = mainView.root

  override def stopApp(): Unit =
    super.stopApp()
