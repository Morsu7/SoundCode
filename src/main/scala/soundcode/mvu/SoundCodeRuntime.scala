package soundcode.mvu;

class SoundCodeRuntime(
    initialModel: AppModel,
    render: AppModel => Unit
):
  private var model: AppModel = initialModel

  def dispatch(msg: Msg): Unit =
    val (nextModel, cmd) = Update.update(model, msg)

    if nextModel != model then
      model = nextModel
      render(model)

    cmd.run(dispatch)

  def currentModel: AppModel = model
