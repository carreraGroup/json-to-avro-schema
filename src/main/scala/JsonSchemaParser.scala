package io.carrera.jsontoavroschema
import io.lemonlabs.uri.Uri

case class RootJsonSchema(schemaUri: Option[Uri], schema: JsonSchema)

case class JsonSchema(
                       id: Option[Uri],
                       ref: Option[Uri],
                       title: Option[String]
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
      id <- parseId(obj)
      ref <- parseRef(obj)
      title <- parseString(obj, "title")
    } yield JsonSchema(id, ref, title)

  def parseRef(obj: ujson.Obj): Either[ParserError, Option[Uri]] = {
    parseUri(obj, "$ref")
  }

  def parseId(obj: ujson.Obj): Either[ParserError, Option[Uri]] = {
    parseUri(obj, "$id")
  }

  def parseSchemaUri(obj: ujson.Obj): Either[ParserError, Option[Uri]] = {
    //TODO: The spec says the schema uri must include a scheme. Validate it does.
    // https://tools.ietf.org/html/draft-wright-json-schema-01#section-7
    parseUri(obj, "$schema")
  }

  private def parseUri(value: ujson.Obj, elemName: String): Either[ParserError, Option[Uri]] = {
    val parser = (node: ujson.Value) => {
      for {
        uriStr <- node.strOpt.toRight(ParserError(s"$elemName must be a URI string"))
        uri <- Uri.parseOption(uriStr).toRight(ParserError(s"Invalid $elemName URI"))
      } yield Some(uri)
    }
    parseValue(value, elemName, parser)
  }

  private def parseString(value: ujson.Obj, elemName: String): Either[ParserError, Option[String]] = {
    val parser = (node: ujson.Value) => {
      for {
        result <- node.strOpt.toRight(ParserError(s"$elemName must be a String"))
      } yield Some(result)
    }
    parseValue(value, elemName, parser)
  }

  /** Checks for the existence of an element before running the parser */
  private def parseValue[T](value: ujson.Obj, elemName: String, parser: ujson.Value => Either[ParserError, Option[T]]): Either[ParserError, Option[T]] =
    if (value.obj.keys.exists(k => k == elemName)) {
      val node = value(elemName)
      parser(node)
    }
    else
      Right(None)
}

final case class ParserError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
