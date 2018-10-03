ThisBuild / scalaVersion := "2.12.6"

lazy val nameProject = (project in file("."))
  .settings(
    name := "Battleship",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,

  )