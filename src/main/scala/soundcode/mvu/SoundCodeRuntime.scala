package soundcode.mvu;

class SoundCodeRuntime(
    initialModel: AppModel,
    update: (AppModel, Msg) => AppModel,
    render: AppModel => Unit
):
  private var model: AppModel = initialModel

  def dispatch(msg: Msg): Unit =
    model = update(model, msg)
    render(model)

  def currentModel: AppModel = model
