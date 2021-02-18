package io.carrera.jsontoavroschema
import io.lemonlabs.uri.Uri

case class RootJsonSchema(schemaUri: Option[Uri], schema: JsonSchema)

case class JsonSchema(
                       id: Option[Uri],
                       ref: Option[Uri],
                       title: Option[String],
                       desc: Option[String],
                       multipleOf: Option[Double]
                     )

object JsonSchemaParser {

  def parse(value: ujson.Value): Either[Throwable, RootJsonSchema] =
    for {
      root <- value.objOpt.toRight(ParserError("schema must be an object"))
      schemaUri <- parseSchemaUri(root)
      // The schema *should* be used to determine how to parse the rest of the document
      // For now, we are just assuming it's a draft 6 document
      schema <- parseSubSchema(root)
    } yield RootJsonSchema(schemaUri, schema)

  /**
   * $schema uri MUST ONLY be in the root schema,
   * so this is our recursive descent function
   * that ignores it.
   * */
  def parseSubSchema(obj: ujson.Obj): Either[ParserError, JsonSchema] =
    for {
      id <- parseUri(obj, "$id")
      ref <- parseUri(obj, "$ref")
      title <- parseString(obj, "title")
      desc <- parseString(obj, "description")
      multipleOf <- parseMultipleOf(obj)
    } yield JsonSchema(id, ref, title, desc, multipleOf)

  def parseSchemaUri(obj: ujson.Obj): Either[ParserError, Option[Uri]] = {
    //TODO: The spec says the schema uri must include a scheme. Validate it does.
    // https://tools.ietf.org/html/draft-wright-json-schema-01#section-7
    parseUri(obj, "$schema")
  }

  def parseMultipleOf(obj: ujson.Obj) =
    for {
      num <- parseNumber(obj, "multipleOf")
      result <- num match {
        case Some(v) => if (v <= 0) Left(ParserError("multipleOf must be > 0")) else Right(num)
        case None => Right(None)
      }
    } yield result

  private def parseUri(value: ujson.Obj, elemName: String): Either[ParserError, Option[Uri]] = {
    val parser = (node: ujson.Value) => {
      for {
        uriStr <- node.strOpt.toRight(ParserError(s"$elemName must be a URI string"))
        uri <- Uri.parseOption(uriStr).toRight(ParserError(s"Invalid $elemName URI"))
      } yield Some(uri)
    }
    runParser(value, elemName, parser)
  }

  private def parseNumber(obj: ujson.Obj, elemName: String): Either[ParserError, Option[Double]] = {
    val parser = (node: ujson.Value) => {
      for {
        num <- node.numOpt.toRight(ParserError(s"$elemName must be a number"))
      } yield Some(num)
    }
    runParser(obj, elemName, parser)
  }

  private def parseString(value: ujson.Obj, elemName: String): Either[ParserError, Option[String]] = {
    val parser = (node: ujson.Value) => {
      for {
        result <- node.strOpt.toRight(ParserError(s"$elemName must be a String"))
      } yield Some(result)
    }
    runParser(value, elemName, parser)
  }

  /** Checks for the existence of an element before running the parser */
  private def runParser[T](value: ujson.Obj, elemName: String, parser: ujson.Value => Either[ParserError, Option[T]]): Either[ParserError, Option[T]] =
    if (value.obj.keys.exists(k => k == elemName)) {
      val node = value(elemName)
      parser(node)
    }
    else
      Right(None)
}

final case class ParserError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
