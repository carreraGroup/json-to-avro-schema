name := "json-to-avro-schema"

version := "0.1"

scalaVersion := "2.13.4"

idePackagePrefix := Some("io.carrera.jsontoavroschema")

libraryDependencies += "com.lihaoyi" %% "upickle" % "1.2.3"
libraryDependencies += "io.lemonlabs" % "scala-uri_2.13" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test

scalacOptions += "-Werror"
