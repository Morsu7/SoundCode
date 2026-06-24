package soundcode.ui

import javafx.application.Platform
import javafx.scene.control.Label
import javafx.scene.layout.{HBox, VBox}
import org.fxmisc.richtext.CodeArea
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

class SoundCodeUISpec extends AnyFunSuite:
  val initialCode =
    """note("c4 a4").sound("piano")
      |sound("hb hd hh")""".stripMargin

  // ensures tests create and inspect JavaFX components on the JavaFX Application Thread.
  private def onFxThread[A](body: => A): A =
    val result = AtomicReference[A]()
    val error = AtomicReference[Throwable]()
    val latch = CountDownLatch(1)

    try Platform.startup(() => ())
    catch case _: IllegalStateException => ()

    Platform.runLater(() =>
      try result.set(body)
      catch case throwable: Throwable => error.set(throwable)
      finally latch.countDown()
    )

    latch.await()

    if error.get() != null then throw error.get()
    result.get()

  test("main view has a root"):
    onFxThread:
      val view = MainView()

      assert(view.root != null)

  test("block editor exposes the current code without changing it"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())

      assert(editor.currentCode == initialCode)

  test("block editor creates one visual row for each line of code"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]

      assert(blocksBox.getChildren.size == 2)

  test("block editor shows progressive line numbers"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]

      val firstRow = blocksBox.getChildren.get(0).asInstanceOf[HBox]
      val secondRow = blocksBox.getChildren.get(1).asInstanceOf[HBox]

      val firstLabel = firstRow.getChildren.get(0).asInstanceOf[Label]
      val secondLabel = secondRow.getChildren.get(0).asInstanceOf[Label]

      assert(firstLabel.getText == "1")
      assert(secondLabel.getText == "2")

  test("line rows use the configured visual height"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]
      val row = blocksBox.getChildren.get(0).asInstanceOf[HBox]

      assert(row.getPrefHeight == 34)
      assert(row.getMinHeight == 34)
      assert(row.getMaxHeight == 34)

  test("line number column uses the same width as the line height"):
    onFxThread:
      val editor = BlockEditorView(initialCode, _ => ())
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]
      val row = blocksBox.getChildren.get(0).asInstanceOf[HBox]
      val lineNumber = row.getChildren.get(0).asInstanceOf[Label]

      assert(lineNumber.getPrefWidth == 34)
      assert(lineNumber.getMinWidth == 34)
      assert(lineNumber.getMaxWidth == 34)

  test("changing a line editor notifies with the full current code"):
    onFxThread:
      var lastChange = ""

      val editor = BlockEditorView(initialCode, code => lastChange = code)
      val blocksBox = editor.root.content.value.asInstanceOf[VBox]
      val firstRow = blocksBox.getChildren.get(0).asInstanceOf[HBox]
      val firstCodeArea = firstRow.getChildren.get(1).asInstanceOf[CodeArea]

      firstCodeArea.replaceText("changed")

      assert(lastChange == "changed\nsound(\"hb hd hh\")")
