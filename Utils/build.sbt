name := "Utils"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(

  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "it.unimi.dsi" % "fastutil" % "7.2.0",
  "com.googlecode.javaewah" % "JavaEWAH" % "1.1.6",
  "org.apache.lucene" % "lucene-core" % "4.10.3",
  "org.roaringbitmap" % "RoaringBitmap" % "0.6.44"
)

