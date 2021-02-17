package io.carrera.jsontoavroschema

import scala.io.Source
import scala.util.Using

object Application extends App {
  getInputFilePath(args) match {
    case Some(path) =>
      for {
        content <- loadFile(path)
        value = readJson(content)
      } yield value
    case None => println("Usage: sbt \"run inputFile\"")
  }

  def readJson(content: String) = {
    ujson.read(content)
  }

  def loadFile(path: String) =
    Using(Source.fromFile(path)) { source =>
      source.getLines.mkString
    }

  def getInputFilePath(args: Array[String]) =
    args.headOption

}
