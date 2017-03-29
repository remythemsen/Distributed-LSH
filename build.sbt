name := "Distributed LSH"

version := "1.0"

scalaVersion := "2.12.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.4.17",
  "com.typesafe.akka" %% "akka-remote" % "2.4.17"
)

lazy val DistributedLSH = project in file(".") dependsOn Repetition

lazy val Utils = project in file("Utils")

lazy val RecallTest = project in file("RecallTest") dependsOn(DistributedLSH, Repetition, Utils)

lazy val MicroBenchmark = project in file("MicroBenchmark") dependsOn(DistributedLSH, Repetition, Utils) enablePlugins JmhPlugin

lazy val Repetition = project in file("Repetition") dependsOn Utils

