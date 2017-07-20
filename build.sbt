name := "Distributed LSH"

version := "1.0"

scalaVersion := "2.12.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.5.1",
  "com.typesafe.akka" %% "akka-remote" % "2.5.1",
  "it.unimi.dsi" % "fastutil" % "7.2.0",
  "org.apache.lucene" % "lucene-core" % "4.10.3",
  "com.googlecode.javaewah" % "JavaEWAH" % "1.1.6",
  "org.roaringbitmap" % "RoaringBitmap" % "0.6.44"

)

libraryDependencies += "commons-io" % "commons-io" % "2.5"

lazy val DistributedLSH = project in file(".") dependsOn RepetitionHandler

lazy val Utils = project in file("Utils")

lazy val RecallTest = project in file("RecallTest") dependsOn(DistributedLSH, RepetitionHandler, Utils)

lazy val MicroBenchmark = project in file("MicroBenchmark") dependsOn(DistributedLSH, RepetitionHandler, Utils) enablePlugins JmhPlugin

lazy val Preprocess = project in file("Preprocess") dependsOn Utils

lazy val KNNFileBuilder = project in file("KNNFileBuilder") dependsOn Utils

lazy val RepetitionHandler = project in file("RepetitionHandler") dependsOn Utils
