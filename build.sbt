name := "project-2b-Dadelinee-personal"

version := "0.4"

scalaVersion := "3.0.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Yexplicit-nulls", "-language:strictEquality")

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.9"  % Test,
  "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
)

scalacOptions ++= Seq("-rewrite", "-new-syntax")

