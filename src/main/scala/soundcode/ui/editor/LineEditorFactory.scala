package soundcode.ui.editor

import javafx.scene.input.{KeyCode, KeyEvent}
import org.fxmisc.richtext.InlineCssTextArea
import org.fxmisc.richtext.util.UndoUtils
import scalafx.application.Platform

private[editor] object LineEditorFactory:
  def create(
      text: String,
      lineHeight: Int,
      onSplit: InlineCssTextArea => Unit,
      onMergePrevious: InlineCssTextArea => Unit,
      onMergeNext: InlineCssTextArea => Unit,
      onMoveFocus: (InlineCssTextArea, Int) => Unit,
      onFocus: InlineCssTextArea => Unit
  ): InlineCssTextArea =
    new InlineCssTextArea:
      private var highlightScheduled = false

      private def scheduleHighlight(): Unit =
        if !highlightScheduled then
          highlightScheduled = true

          Platform.runLater {
            highlightScheduled = false
            SyntaxHighlighter.applyTo(this)
          }

      replaceText(text.replace("\r", "").replace("\n", ""))
      setUndoManager(UndoUtils.plainTextUndoManager(this))
      setPrefHeight(lineHeight)
      setMinHeight(lineHeight)
      setMaxHeight(lineHeight)
      setMinWidth(0)
      setPrefWidth(0)
      setMaxWidth(Double.MaxValue)
      setStyle(
        """
          |-fx-font-family: 'Consolas';
          |-fx-font-size: 15px;
          |-fx-background-color: #f4f4f5;
          |-fx-padding: 6 0 0 0;
          |""".stripMargin
      )
      textProperty().addListener { (_, _, _) =>
        scheduleHighlight()
      }
      focusedProperty().addListener { (_, _, focused) =>
        if focused then onFocus(this)
        else deselect()
      }
      addEventFilter(
        KeyEvent.KEY_PRESSED,
        (event: KeyEvent) =>
          event.getCode match
            case KeyCode.ENTER =>
              event.consume()
              onSplit(this)
            case KeyCode.BACK_SPACE if getCaretPosition == 0 =>
              event.consume()
              onMergePrevious(this)
            case KeyCode.DELETE if getCaretPosition == getLength =>
              event.consume()
              onMergeNext(this)
            case KeyCode.UP =>
              event.consume()
              onMoveFocus(this, -1)
            case KeyCode.DOWN =>
              event.consume()
              onMoveFocus(this, 1)
            case _ =>
      )
      AutoPairingSupport.install(this)
      scheduleHighlight()
