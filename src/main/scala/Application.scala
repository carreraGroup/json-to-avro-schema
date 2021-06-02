package io.carrera.jsontoavroschema

import java.io.{File, PrintWriter, Writer}
import scala.io.Source
import scala.util.Using
import Console.{GREEN, RED, RESET}
import scala.annotation.tailrec

object Application extends App {
  private val usage = "Usage: sbt \"run [-n \"com.example\"] inputFile outputDir\""

  parseArgs(args.toList) match {
    case Left(msg) =>
      logError(msg)
      log(usage)
    case Right(options) =>
      val inputFilePath = getInputFilePath(options)
      run(inputFilePath, getNamespace(options)) match {
        case Right(output) =>
          logSuccess("success")
          output.map { case (name, record) =>
            Using(new PrintWriter(new File(s"${getOutputDir(options)}/$name.avsc"))) { writer =>
              writeRecord(record, writer)
            }
          }
        case Left(err) => logError(err.toString)
      }
  }

  def run(inputFilePath: String, namespace: Option[String]) =
    for {
      content <- loadFile(inputFilePath).toEither
      _ = logSuccess("input loaded")
      inputJson = readJson(content)
      jsonSchema <- JsonSchemaParser.parse(inputJson)
      _ = logSuccess("parsed")
      avroSchemas <- Transpiler.transpile(jsonSchema.schema, namespace)
      outputJson = avroSchemas.map { case (k, v) => (k, AvroWriter.toJson(v))}
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
      case _ :: Nil => Left("outputDir is required")
      case filePath :: outDir :: Nil => Right(accumulator + ("inputFile" -> filePath) + ("outputDir" -> outDir))
      case ("--namespace" | "-n") :: value :: tail => parseArgs(tail, accumulator + ("namespace" -> value))
      case x :: _ => Left (s"unrecognized option: $x")
    }

  private def getInputFilePath(options: Map[String,String]) =
    options("inputFile")

  private def getNamespace(options: Map[String,String]) =
    options.get("namespace")

  private def getOutputDir(options: Map[String,String]) =
    options("outputDir")

  //Everything goes to the error stream so we can write results to stdout
  private def logError(msg: String): Unit =
    Console.err.println(s"$RESET$RED$msg$RESET")

  private def logSuccess(msg: String): Unit =
    Console.err.println(s"$RESET$GREEN$msg$RESET")

  private def log(msg: String): Unit =
    Console.err.println(s"$RESET$msg$RESET")
}
