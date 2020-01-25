
lazy val commonSettings = Seq(
    scalaVersion := "2.13.1",

    // THIS BREAKS INPUT READER!
    //fork in run := true,

    // Show warnings
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Xlint"),

    // Dependencies
    // Scalatest
    //libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4",
    //libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    
    // Scala Reflect
    //libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
    
)

lazy val main = (project in file(""))
    .settings(
        commonSettings,
        name := "mad-diagram-toolkit"
    )
