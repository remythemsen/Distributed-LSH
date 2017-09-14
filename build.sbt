name := "LSHBundle"

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

lazy val LSHBundle = project in file(".") dependsOn RepetitionHandler

lazy val LSH = project in file("LSH") dependsOn Utils

lazy val LSHRecall = project in file("LSHRecall") dependsOn(LSHBundle, RepetitionHandler, Utils)

lazy val EmbeddingRecall = project in file("EmbeddingRecall") dependsOn(Utils, KNN)

lazy val Embedding = project in file("Embedding") dependsOn Utils

lazy val MicroBenchmark = project in file("MicroBenchmark") dependsOn(LSHBundle, RepetitionHandler, Utils) enablePlugins JmhPlugin

lazy val RepetitionHandler = project in file("RepetitionHandler") dependsOn Utils

lazy val Utils = project in file("Utils")

lazy val KNN = project in file("KNN") dependsOn Utils
