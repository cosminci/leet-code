lazy val root = project
  .in(file("."))
  .settings(
    name := "leet-code",
    version := "0.1.0",

    scalaVersion := "3.0.2",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.2.9",
      "org.typelevel" %% "cats-core" % "2.6.1"
    )
  )
