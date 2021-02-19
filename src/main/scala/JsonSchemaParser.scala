package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri

case class RootJsonSchema(schemaUri: Option[Uri], schema: JsonSchema)

case class JsonSchema(
                       id: Option[Uri],
                       ref: Option[Uri],
                       title: Option[String],
                       desc: Option[String],
                       definitions: Map[String, JsonSchema],
                       default: Option[ujson.Value],
                       multipleOf: Option[Double],
                       maximum: Option[Double],
                       exclusiveMaximum: Option[Double],
                       minimum: Option[Double],
                       exclusiveMinimum: Option[Double],
                       maxLength: Option[Int],
                       minLength: Int,
                       pattern: Option[String],
                       items: Seq[JsonSchema],
                       maxItems: Option[Int],
                       minItems: Int,
                       uniqueItems: Boolean,
                       required: Seq[String],
                       properties: Map[String, JsonSchema],
                       const: Option[ujson.Value],
                       types: Seq[String],
                       enum: Seq[ujson.Value],
                       allOf: Seq[JsonSchema],
                       anyOf: Seq[JsonSchema],
                       oneOf: Seq[JsonSchema],
                       not: Option[JsonSchema],
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
      id <- parseUriOpt(obj, "$id")
      ref <- parseUriOpt(obj, "$ref")
      title <- parseStringOpt(obj, "title")
      desc <- parseStringOpt(obj, "description")
      definitions <- parseSchemaMap(obj, "definitions")
      default <- parseAnyOpt(obj, "default")
      multipleOf <- parseNumberGreaterThanZeroOpt(obj, "multipleOf")
      max <- parseNumberOpt(obj, "maximum")
      exclMax <- parseNumberOpt(obj, "exclusiveMaximum")
      min <- parseNumberOpt(obj, "minimum")
      exclMin <- parseNumberOpt(obj, "exclusiveMinimum")
      maxLen <- parsePositiveIntegerOpt(obj, "maxLength")
      minLen <- parsePositiveIntegerWithDefaultZero(obj, "minLength")
      pattern <- parsePatternOpt(obj)
      items <- parseItems(obj)
      maxItems <- parsePositiveIntegerOpt(obj, "maxItems")
      minItems <- parsePositiveIntegerWithDefaultZero(obj, "minItems")
      uniqueItems <- parseUniqueItems(obj)
      required <- parseRequired(obj)
      properties <- parseSchemaMap(obj, "properties")
      const <- parseAnyOpt(obj, "const")
      types <- parseTypes(obj)
      enum <- parseEnum(obj)
      allOf <- parseSchemaArray(obj, "allOf")
      anyOf <- parseSchemaArray(obj, "anyOf")
      oneOf <- parseSchemaArray(obj, "oneOf")
      not <- parseSchemaOpt(obj, "not")
    } yield
      JsonSchema(
        id,
        ref,
        title,
        desc,
        definitions,
        default,
        multipleOf,
        max,
        exclMax,
        min,
        exclMin,
        maxLen,
        minLen,
        pattern,
        items,
        maxItems,
        minItems,
        uniqueItems,
        required,
        properties,
        const,
        types,
        enum,
        allOf,
        anyOf,
        oneOf,
        not,
      )

  private def parseItems(value: ujson.Obj) = {
    val parser = (node: ujson.Value) => {
      for {
        items <- node match {
          case n: ujson.Obj => parseSubSchema(n).flatMap(schema => Right(Seq(schema)))
          case ujson.Arr(a) => parseSchemas(a, "items")
          case _ => Left(ParserError("items must be an object or array"))
        }
      } yield items
    }
    runSeqParser(value, "items", parser)
  }

  private def parseRequired(value: ujson.Obj)= {
    val parser = (node: ujson.Value) => {
      for {
        required <-
          node
            .arrOpt
            .toRight(ParserError("required must be an array"))
            .map(_.toSeq)
        props <- required.foldLeft(Right(Seq[String]()).withLeft[ParserError]) { case (acc, cur) =>
          for {
            last <- acc
            prop <- cur.strOpt.toRight(ParserError("required array contents must be strings"))
          } yield last :+ prop
        }
      } yield props
    }
    runSeqParser(value, "required", parser)
  }

  private def parseTypes(obj: ujson.Obj) = {
    val parser = (node: ujson.Value) => {
      for {
        types <- node match {
          case ujson.Str(s) => Right(Seq(s))
          case ujson.Arr(a) => a.foldLeft(Right(Seq[String]()).withLeft[ParserError]) { case (acc, cur) =>
            for {
              last <- acc
              t <- cur.strOpt.toRight(ParserError("type value must be a string"))
            } yield last :+ t
          }
          case _ => Left(ParserError("type must be a string or array"))
        }
      } yield types
    }
    runSeqParser(obj, "type", parser)
  }

  private def parseEnum(obj: ujson.Obj) = {
    val parser = (node: ujson.Value) => {
      for {
        enum <-
          node
            .arrOpt
            .toRight(ParserError("enum must be an array"))
            .map(_.toSeq)
      } yield enum
    }
    runSeqParser(obj, "enum", parser)
  }

  private def parseSchemaUri(obj: ujson.Obj): Either[ParserError, Option[Uri]] = {
    //TODO: The spec says the schema uri must include a scheme. Validate it does.
    // https://tools.ietf.org/html/draft-wright-json-schema-01#section-7
    parseUriOpt(obj, "$schema")
  }

  private def parseUniqueItems(obj: ujson.Obj) =
    for {
      maybeBool <- parseBoolOpt(obj, "uniqueItems")
    } yield maybeBool match {
      case Some(b) => b
      case None => false
    }

  private def parseSchemaMap(obj: ujson.Obj, elemName: String): Either[ParserError, Map[String,JsonSchema]] = {
    val parser = (node: ujson.Value) => {
      for {
        rawProps <- node.objOpt.toRight(ParserError(s"$elemName must be an object"))
        props <- rawProps.foldLeft(Right(Map[String,JsonSchema]()).withLeft[ParserError]) { case (acc, (k, v)) =>
          for {
            last <- acc
            schema <- parseSchema(v, s"$elemName values must be objects")
          } yield last + (k -> schema)
        }
      } yield props
    }
    runMapParser(obj, elemName, parser)
  }

  private def parseSchemaArray(value: ujson.Obj, elemName: String) = {
    val parser = (node: ujson.Value) => {
      for {
        items <- node.arrOpt.toRight(ParserError(s"$elemName must be an array"))
        schemas <- parseSchemas(items, elemName)
      } yield schemas
    }
    runSeqParser(value, elemName, parser)
  }

  private def parseSchemas(items: IterableOnce[ujson.Value], elemName: String) = {
    items.iterator.foldLeft(Right(Seq[JsonSchema]()).withLeft[ParserError]) { case (acc, cur) =>
      for {
        last <- acc
        schema <- parseSchema(cur, s"$elemName array contents must be objects")
      } yield last :+ schema
    }
  }

  private def parseSchemaOpt(obj: ujson.Obj, elemName: String) =
    runOptParser(obj, elemName, optParser(parseSchema)(elemName))

  private def parseSchema(value: ujson.Value, errMsg: String) = {
    for {
      obj <- value.objOpt.toRight(ParserError(errMsg))
      schema <- parseSubSchema(obj)
    } yield schema
  }

  private def parsePatternOpt(obj: ujson.Obj) = {
    //TODO: verify value is a ECMA 262 regex
    parseStringOpt(obj, "pattern")
  }

  private def parsePositiveIntegerWithDefault(default: Int)(obj: ujson.Obj, elemName: String) =
    for {
      num <- parsePositiveIntegerOpt(obj, elemName)
      result = num.getOrElse(default)
    } yield result

  private def parsePositiveIntegerWithDefaultZero =
    parsePositiveIntegerWithDefault(0) _

  //TODO: use optParser
  private def parsePositiveIntegerOpt(obj: ujson.Obj, elemName: String) =
    for {
      num <- parseIntegerOpt(obj, elemName)
      result <- num match {
        case Some(n) =>
          if (n < 0)
            Left(ParserError(s"$elemName must be >= 0"))
          else
            Right(Some(n))
        case None => Right(None)
      }
    } yield result

  private def parseIntegerOpt(obj: ujson.Obj, elemName: String) =
    runOptParser(obj, elemName, optParser(parseInteger)(elemName))

  private def parseNumberGreaterThanZeroOpt(obj: ujson.Obj, elemName: String) =
    for {
      num <- parseNumberOpt(obj, elemName)
      result <- num match {
        case Some(v) => if (v <= 0) Left(ParserError(s"$elemName must be > 0")) else Right(num)
        case None => Right(None)
      }
    } yield result

  private def parseNumberOpt(obj: ujson.Obj, elemName: String) =
    runOptParser(obj, elemName, optParser(parseNumber)(elemName))

  private def parseBoolOpt(value: ujson.Obj, elemName: String) =
    runOptParser(value, elemName, optParser(parseBool)(elemName))

  private def parseUriOpt(value: ujson.Obj, elemName: String): Either[ParserError, Option[Uri]] =
    runOptParser(value, elemName, optParser(parseUri)(elemName))

  private def parseStringOpt(value: ujson.Obj, elemName: String) =
    runOptParser(value, elemName, optParser(parseString)(elemName))

  private def parseAnyOpt(value: ujson.Obj, elemName: String) =
    runOptParser(value, elemName, optParser(parseIdentity)(elemName))

  private def parseInteger(value: ujson.Value, elemName: String) =
    for {
      num <- parseNumber(value, elemName)
    } yield num.toInt

  private def parseNumber(value: ujson.Value, elemName: String) =
    for {
      num <- value.numOpt.toRight(ParserError(s"$elemName must be a number"))
    } yield num

  private def parseBool(value: ujson.Value, elemName: String) =
    for {
      result <- value.boolOpt.toRight(ParserError(s"$elemName must be a boolean"))
    } yield result

  private def parseUri(value: ujson.Value, elemName: String) =
    for {
      uriStr <- parseString(value, elemName)
      uri <- Uri.parseOption(uriStr).toRight(ParserError(s"Invalid $elemName URI"))
    } yield uri

  private def parseString(value: ujson.Value, elemName: String) =
    for {
      result <- value.strOpt.toRight(ParserError(s"$elemName must be a String"))
    } yield result

  private def parseIdentity(value: ujson.Value, elemName: String) =
    Right(value).withLeft[ParserError]

  private def optParser[T](parser: (ujson.Value,String) => Either[ParserError,T])(elemName: String)(value: ujson.Value) = {
    for {
      result <- parser(value, elemName)
    } yield Option(result)
  }

  //TODO: Figure out how to make a function that's generic over several containers
  /** Checks for the existence of an element before running the parser */
  private def runOptParser[T](value: ujson.Obj, elemName: String, parser: ujson.Value => Either[ParserError, Option[T]]) =
    if (value.obj.keys.exists(k => k == elemName)) {
      val node = value(elemName)
      parser(node)
    }
    else
      Right(None)

  /** Checks for the existence of an element before running the parser */
  private def runSeqParser[T](value: ujson.Obj, elemName: String, parser: ujson.Value => Either[ParserError, Seq[T]]) =
    if (value.obj.keys.exists(k => k == elemName)) {
    val node = value(elemName)
    parser(node)
  }
  else
    Right(Seq())

  private def runMapParser[T](value: ujson.Obj, elemName: String, parser: ujson.Value => Either[ParserError, Map[String,T]]): Either[ParserError, Map[String,T]] =
    if (value.obj.keys.exists(k => k == elemName)) {
      val node = value(elemName)
      parser(node)
    }
    else
      Right(Map())
}

final case class ParserError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
