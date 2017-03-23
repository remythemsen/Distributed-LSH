name := "Recall Test"

organization := "dk.distributed_lsh"

version := "1.0"

homepage := Some(url("https://github.com/remythemsen/Distributed_LSH"))

startYear := Some(2017)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/remythemsen/Distributed_LSH"),
    "scm:git:https://github.com/remythemsen/Distributed_LSH.git",
    Some("scm:git:git@github.com:remythemsen/Distributed_LSH.git")
  )
)

scalaVersion := "2.12.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.2.0",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

maintainer := "Remy Themsen <remt@itu.dk>"

dockerRepository := Some("remeeh")

dockerExposedPorts := Seq(2552)

dockerBaseImage := "java"
enablePlugins(JavaAppPackaging)
