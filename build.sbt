name := "streamBase64"

organization := "com.github.stream_base64"

version := "1.0"

scalaVersion := "2.12.10"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)
