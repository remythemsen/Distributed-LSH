logLevel := Level.Warn

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.0.3")

addSbtPlugin("se.marcuslonnberg" % "sbt-docker" % "1.2.0")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.24")

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
