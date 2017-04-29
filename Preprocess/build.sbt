name := "Preprocess"

version := "latest"

scalaVersion := "2.12.1"

startYear := Some(2017)

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

homepage := Some(url("https://github.com/remythemsen/Distributed_LSH"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/remythemsen/Distributed_LSH"),
    "scm:git:https://github.com/remythemsen/Distributed_LSH.git",
    Some("scm:git:git@github.com:remythemsen/Distributed_LSH.git")
  )
)

maintainer := "Remy Themsen <remt@itu.dk>"

version in Docker := "latest"

dockerRepository := Some("remeeh")

dockerBaseImage := "java"
enablePlugins(JavaAppPackaging)
