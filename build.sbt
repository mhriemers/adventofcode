ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "com.riemers"

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "1.4.0",
    "org.typelevel" %% "cats-effect" % "1.0.0",
    "io.monix" %% "monix" % "3.0.0-RC2",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  )
)

lazy val adventofcode = (project in file("."))
  .aggregate(common, day1, day2)

lazy val common = (project in file("common"))
  .settings(commonSettings)

lazy val day1 = (project in file("day1"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val day2 = (project in file("day2"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val day3 = (project in file("day3"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val day4 = (project in file("day4"))
  .settings(commonSettings)
  .dependsOn(common)