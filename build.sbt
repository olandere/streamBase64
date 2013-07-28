name := "streamBase64"

organization <<= name

version := "1.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-optimise", "-feature")

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.9.1" % "test",
    "org.scala-lang" % "scala-reflect" % "2.10.2"
)
