name := "Utils"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "it.unimi.dsi" % "fastutil" % "7.2.0",
  "org.apache.lucene" % "lucene-core" % "4.10.3"
)

