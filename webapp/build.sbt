name := "encalmo-webapp"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)

libraryDependencies += "org.encalmo" % "encalmo-structures" % "1.0.0-SNAPSHOT"

resolvers += "Local Maven Repository" at "file:///"+System.getProperty("user.home")+"/.m2/repository"

play.Project.playScalaSettings
