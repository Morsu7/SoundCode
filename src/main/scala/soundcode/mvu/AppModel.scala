package soundcode.mvu

final case class AppModel(
    code: String = """|note("c4 a4").sound("piano")
           |sound("hb hd hh")
           |""".stripMargin.trim,
    currentBeat: Double = 0.0,
    isPlaying: Boolean = false
)
