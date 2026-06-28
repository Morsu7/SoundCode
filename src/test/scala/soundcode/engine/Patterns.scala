package soundcode.engine

import soundcode.domain.*

object Patterns {
  import soundcode.domain.AggregationPattern.*

  val pos = TextPosition(0, 0)

  val `<bd sn>`: Pattern = List(
    Seq(AlternationPattern(List(
      Seq(Sound.SampleInText(Sample("bd"), pos)),
      Seq(Sound.SampleInText(Sample("sn"), pos))
    )))
  )

  val `bd Hh Sn Hh`: Pattern = List(
    Seq(
      Sound.SampleInText(Sample("bd"), pos),
      Sound.SampleInText(Sample("hh"), pos),
      Sound.SampleInText(Sample("sn"), pos),
      Sound.SampleInText(Sample("hh"), pos)
    )
  )

  val `bd Hh`: Pattern = List(
    Seq(
      Sound.SampleInText(Sample("bd"), pos),
      Sound.SampleInText(Sample("hh"), pos)
    )
  )

  val `c f g`: Pattern = List(
    Seq(
      Sound.NoteInText(Note("c"), pos),
      Sound.NoteInText(Note("f"), pos),
      Sound.NoteInText(Note("g"), pos)
    )
  )

  val `bd hh sn hh [hh , sn < bd hh > ]`: Pattern = List(
    Seq(
      Sound.SampleInText(Sample("bd"), pos),
      Sound.SampleInText(Sample("hh"), pos),
      Sound.SampleInText(Sample("sn"), pos),
      Sound.SampleInText(Sample("hh"), pos),
      SubPattern(List(
        Seq(
          Sound.SampleInText(Sample("hh"), pos)
        ),
        Seq(
          Sound.SampleInText(Sample("sn"), pos),
          AlternationPattern(List(
            Seq(
              Sound.SampleInText(Sample("bd"), pos),
              Sound.SampleInText(Sample("hh"), pos)
            )
          ))
        )
      ))
    )
  )

  val `bd [hh [sn cp]]`: Pattern = List(
    Seq(
      Sound.SampleInText(Sample("bd"), pos),
      SubPattern(List(
        Seq(
          Sound.SampleInText(Sample("hh"), pos),
          SubPattern(List(
            Seq(
              Sound.SampleInText(Sample("sn"), pos),
              Sound.SampleInText(Sample("cp"), pos)
            )
          ))
        )
      ))
    )
  )

  val `<bd [hh sn]> cp`: Pattern = List(
    Seq(
      AlternationPattern(List(
        Seq(
          Sound.SampleInText(Sample("bd"), pos),
          // Le parentesi quadre [] creano un SubPattern!
          SubPattern(List(Seq(
            Sound.SampleInText(Sample("hh"), pos),
            Sound.SampleInText(Sample("sn"), pos)
          )))
        )
      )),
      Sound.SampleInText(Sample("cp"), pos)
    )
  )

  val `[bd hh, cp cp cp]`: Pattern = List(
    Seq(Sound.SampleInText(Sample("bd"), pos), Sound.SampleInText(Sample("hh"), pos)), // Layer 1 (2 step)
    Seq(Sound.SampleInText(Sample("cp"), pos), Sound.SampleInText(Sample("cp"), pos), Sound.SampleInText(Sample("cp"), pos)) // Layer 2 (3 step)
  )

  val `bd hh sn hh < bd hh , hh , hh >`: Pattern = List(
    Seq(
      Sound.SampleInText(Sample("bd"), pos),
      Sound.SampleInText(Sample("hh"), pos),
      Sound.SampleInText(Sample("sn"), pos),
      Sound.SampleInText(Sample("hh"), pos),
      AlternationPattern(List(
        Seq(Sound.SampleInText(Sample("bd"), pos), Sound.SampleInText(Sample("hh"), pos)), // Scelta 0 (Giro 0, 3, 6...)
        Seq(Sound.SampleInText(Sample("hh"), pos)), // Scelta 1 (Giro 1, 4, 7...)
        Seq(Sound.SampleInText(Sample("hh"), pos)) // Scelta 2 (Giro 2, 5, 8...)
      ))
    )
  )

  val `c f [ g h c# ]`: Pattern = List(
    Seq(
      Sound.NoteInText(Note("c"), pos),
      Sound.NoteInText(Note("f"), pos),
      SubPattern(List(
        Seq(
          Sound.NoteInText(Note("g"), pos),
          Sound.NoteInText(Note("h"), pos),
          Sound.NoteInText(Note("c#"), pos)
        )
      ))
    )
  )

  val `gain 3 4 5 6 7`: Pattern = List(
    Seq(
      Effect.Gain(3),
      Effect.Gain(4),
      Effect.Gain(5),
      Effect.Gain(6),
      Effect.Gain(7)
    )
  )

  val `gain 3 5`: Pattern = List(
    Seq(
      Effect.Gain(3),
      Effect.Gain(5)
    )
  )

  val `room 6`: Pattern = List(
    Seq(Effect.Room(6))
  )

  val `room 6 7 4`: Pattern = List(
    Seq(
      Effect.Room(6),
      Effect.Room(7),
      Effect.Room(4)
    )
  )

  val `<bd bd hh bd rim bd hh bd>`: Pattern = List(
    Seq(
      AlternationPattern(List(
        Seq(
          Sound.SampleInText(Sample("bd"), pos),
          Sound.SampleInText(Sample("bd"), pos),
          Sound.SampleInText(Sample("hh"), pos),
          Sound.SampleInText(Sample("bd"), pos),
          Sound.SampleInText(Sample("rim"), pos),
          Sound.SampleInText(Sample("bd"), pos),
          Sound.SampleInText(Sample("hh"), pos),
          Sound.SampleInText(Sample("bd"), pos)
        )
      ))
    )
  )

  val `room(4 5 [4] , <4 5 6>)`: Pattern = List(
    Seq(
      Effect.Room(4),
      Effect.Room(5),
      SubPattern(List(
        Seq(Effect.Room(4))
      ))
    ),
    Seq(
      AlternationPattern(List(
        Seq(
          Effect.Room(4),
          Effect.Room(5),
          Effect.Room(6)
        )
      ))
    )
  )

  val `bd sn hh`: Pattern = List(
    Seq(
      Sound.SampleInText(Sample("bd"), pos),
      Sound.SampleInText(Sample("sn"), pos),
      Sound.SampleInText(Sample("hh"), pos)
    )
  )

  val `c f`: Pattern = List(
    Seq(
      Sound.NoteInText(Note("c"), pos),
      Sound.NoteInText(Note("f"), pos)
    )
  )
}