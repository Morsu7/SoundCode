package soundcode.ui

import soundcode.mvu.Msg

class MainViewSpec extends UITestSupport:
  test("main view has a root"):
    onFxThread:
      val view = MainView(_ => ())

      assert(view.root != null)
