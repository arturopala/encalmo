name := "encalmo-webapp"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)

libraryDependencies ++= Seq(
  "org.encalmo" % "encalmo-structures" % "1.0.0-SNAPSHOT",
  "org.webjars" %% "webjars-play" % "2.2.1",
  "org.webjars" % "bootstrap" % "3.0.3"
)

resolvers += "Local Maven Repository" at "file:///"+System.getProperty("user.home")+"/.m2/repository"

play.Project.playScalaSettings
