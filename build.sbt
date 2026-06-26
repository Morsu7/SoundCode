name := "PPS-26-SoundCode"

version := "0.1.0-SNAPSHOT"

scalaVersion := "3.8.4"

// Definisce il progetto radice nella cartella corrente (".")
lazy val root = (project in file("."))
  .settings(
    // Puoi mettere altre impostazioni specifiche qui se serve
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "com.lihaoyi" %% "fastparse" % "3.1.1",

  "org.scalafx" %% "scalafx" % "21.0.0-R32",
  "org.fxmisc.richtext" % "richtextfx" % "0.11.7"
)
