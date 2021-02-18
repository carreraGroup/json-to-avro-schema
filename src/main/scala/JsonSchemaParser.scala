package io.carrera.jsontoavroschema
import io.lemonlabs.uri.Uri

case class RootJsonSchema(schemaUri: Option[Uri], schema: JsonSchema)

case class JsonSchema(
                       id: Option[Uri],
                       ref: Option[Uri],
                       title: Option[String],
                       desc: Option[String],
                       multipleOf: Option[Double],
                       maximum: Option[Double],
                       exclusiveMaximum: Option[Double],
                       minimum: Option[Double],
                       exclusiveMinimum: Option[Double],
                       maxLength: Option[Int],
                       minLength: Int,
                       pattern: Option[String],
                       items: Seq[JsonSchema]
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
      max <- parseNumber(obj, "maximum")
      exclMax <- parseNumber(obj, "exclusiveMaximum")
      min <- parseNumber(obj, "minimum")
      exclMin <- parseNumber(obj, "exclusiveMinimum")
      maxLen <- parsePositiveInteger(obj, "maxLength")
      minLen <- parseMinLength(obj)
      pattern <- parsePattern(obj)
      items <- parseItems(obj)
    } yield
      JsonSchema(
        id,
        ref,
        title,
        desc,
        multipleOf,
        max,
        exclMax,
        min,
        exclMin,
        maxLen,
        minLen,
        pattern,
        items,
      )

  private def parseItems(value: ujson.Obj) = {
    val elemName = "items"
    val parser = (node: ujson.Value) => {
      for {
        items <- node match {
          case n: ujson.Obj => parseSubSchema(n)
        }
      } yield Seq(items)
    }
    if (value.obj.keys.exists(k => k == elemName)) {
      val node = value(elemName)
      parser(node)
    }
    else
      Right(Seq())
  }

  private def parseSchemaUri(obj: ujson.Obj): Either[ParserError, Option[Uri]] = {
    //TODO: The spec says the schema uri must include a scheme. Validate it does.
    // https://tools.ietf.org/html/draft-wright-json-schema-01#section-7
    parseUri(obj, "$schema")
  }

  private def parseMultipleOf(obj: ujson.Obj) =
    for {
      num <- parseNumber(obj, "multipleOf")
      result <- num match {
        case Some(v) => if (v <= 0) Left(ParserError("multipleOf must be > 0")) else Right(num)
        case None => Right(None)
      }
    } yield result

  private def parseMinLength(obj: ujson.Obj) =
    for {
      num <- parsePositiveInteger(obj, "minLength")
      result = num.getOrElse(0)
    } yield result

  private def parsePattern(obj: ujson.Obj) = {
    //TODO: verify value is a ECMA 262 regex
    parseString(obj, "pattern")
  }

  private def parsePositiveInteger(obj: ujson.Obj, elemName: String) =
    for {
      num <- parseInteger(obj, elemName)
      result <- num match {
        case Some(n) =>
          if (n < 0)
            Left(ParserError(s"$elemName must be >= 0"))
          else
            Right(Some(n))
        case None => Right(None)
      }
    } yield result

  private def parseInteger(obj: ujson.Obj, elemName: String) =
    for {
      num <- parseNumber(obj, elemName)
      result = num.map(_.toInt)
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
