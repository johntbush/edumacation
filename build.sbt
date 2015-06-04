import _root_.sbt.Keys._

name := "meetup"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"

libraryDependencies += "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.1.0"

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4"
