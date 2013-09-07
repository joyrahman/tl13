name := "warmup"

version := "0.1.0"

scalaVersion := "2.10.2"

scalaSource in Compile := file("src")

scalaSource in Test := file("test")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"
