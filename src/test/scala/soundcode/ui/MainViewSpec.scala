package soundcode.ui

class MainViewSpec extends UITestSupport:
  test("main view has a root"):
    onFxThread:
      val view = MainView()

      assert(view.root != null)
