name := "KNNFileBuilder"

version := "1.0"

scalaVersion := "2.12.1"

homepage := Some(url("https://github.com/remythemsen/KNNRecallTest"))

startYear := Some(2017)

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
