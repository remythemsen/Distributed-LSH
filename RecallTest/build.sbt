name := "RecallTest"

organization := "dk.distributed_lsh"

version := "latest"

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

version in Docker := "latest"

dockerExposedPorts := Seq(2553)

dockerRepository := Some("remeeh")

dockerBaseImage := "java"
enablePlugins(JavaAppPackaging)