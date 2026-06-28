package soundcode.mvu

import soundcode.domain.*

final case class AppModel(
    code: String = """|note("c4 a4").sound("piano")
           |sound("hb hd hh")
           |""".stripMargin.trim,
    positions: Set[TextPosition] = Set.empty,
    timelines: List[Seq[Sound]] = List.empty,
    isPlaying: Boolean = false,
    currentBeat: Double = 0.0,

    streams: List[Stream] = List.empty
)
