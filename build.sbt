organization := """com.hunorkovacs"""

name := """koauth-signer"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4",
  "com.hunorkovacs" % "koauth_2.11" % "1.0-SNAPSHOT"
)

