ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.0"

lazy val root = (project in file("."))
  .settings(
    name := "t1"
  )

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

