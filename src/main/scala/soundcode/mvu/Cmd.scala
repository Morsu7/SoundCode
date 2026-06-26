package soundcode.mvu

sealed trait Cmd:
  def run(dispatch: Msg => Unit): Unit

object Cmd:
  case object NoOp extends Cmd:
    override def run(dispatch: Msg => Unit): Unit = println(
      "No command to run."
    )
