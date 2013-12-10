name := "encalmo-webapp"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)

libraryDependencies += "org.encalmo" % "encalmo-structures" % "1.0.0-SNAPSHOT"

play.Project.playScalaSettings
