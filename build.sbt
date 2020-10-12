ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "com.example"

lazy val main = (project in file("."))
  .settings(
    name := "lambda-pi",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0",
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
  )

scalacOptions := Seq("-unchecked", "-deprecation")

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild  / watchBeforeCommand := Watch.clearScreen
