package io.carrera.jsontoavroschema

import java.io.{PrintWriter, Writer}
import scala.io.Source
import scala.util.Using
import Console.{GREEN, RED, RESET}
import scala.annotation.tailrec

object Application extends App {
  //TODO: also require output directory
  private val usage = "Usage: sbt \"run [-n \"com.example\"] inputFile\""

  parseArgs(args.toList) match {
    case Left(msg) =>
      logError(msg)
      log(usage)
    case Right(options) =>
      getInputFilePath(options) match {
        case None =>
          logError("Must specify inputFile")
          log(usage)
        case Some(inputFilePath) =>
          //TODO: run should return a map of (name -> obj)
          run(inputFilePath, getNamespace(options)) match {
            case Right(output) =>
              logSuccess("success")
              //TODO: for each record, write it's file
              Using(new PrintWriter(Console.out)) { writer =>
                writeRecord(output, writer)
              }
            case Left(err) => logError(err.toString)
          }
      }
  }

  def run(inputFilePath: String, namespace: Option[String]) =
    for {
      content <- loadFile(inputFilePath).toEither
      _ = logSuccess("input loaded")
      inputJson = readJson(content)
      jsonSchema <- JsonSchemaParser.parse(inputJson)
      _ = logSuccess("parsed")
      //TODO: change transpile to output a Map(name -> schema)
      avroSchema <- Transpiler.transpile(jsonSchema.schema, namespace)
      outputJson = AvroWriter.toJson(avroSchema)
    } yield outputJson

  def readJson(content: String) =
    ujson.read(content)

  def loadFile(path: String) =
    Using(Source.fromFile(path))(_.mkString)

  def writeRecord(record: ujson.Obj, out: Writer) =
    ujson.writeTo(record, out, indent = 2)

  def parseArgs(args: List[String]): Either[String, Map[String,String]] = {
    if (args.isEmpty)
      Left("inputFile is required")
    else
      parseArgs(args, Map())
  }

  @tailrec
  private def parseArgs(args: List[String], accumulator: Map[String, String]): Either[String, Map[String,String]] =
    args match {
      case Nil => Right(accumulator)
      case filePath :: Nil => Right(accumulator + ("inputFile" -> filePath))
      case ("--namespace" | "-n") :: value :: tail => parseArgs(tail, accumulator + ("namespace" -> value))
      case x :: _ => Left (s"unrecognized option: $x")
    }

  private def getInputFilePath(options: Map[String,String]) =
    options.get("inputFile")

  private def getNamespace(options: Map[String,String]) =
    options.get("namespace")

  //Everything goes to the error stream so we can write results to stdout
  private def logError(msg: String): Unit =
    Console.err.println(s"$RESET$RED$msg$RESET")

  private def logSuccess(msg: String): Unit =
    Console.err.println(s"$RESET$GREEN$msg$RESET")

  private def log(msg: String): Unit =
    Console.err.println(s"$RESET$msg$RESET")
}
