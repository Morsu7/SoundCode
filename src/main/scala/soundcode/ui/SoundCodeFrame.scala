package soundcode.ui

import scalafx.application.JFXApp3
import scalafx.scene.Scene

object SoundCodeFrame extends JFXApp3:
  override def start(): Unit =
    val mainView = MainView()

    stage = new JFXApp3.PrimaryStage:
      title = "SoundCode"
      scene = new Scene(800, 500):
        root = mainView.root

  override def stopApp(): Unit =
    super.stopApp()
