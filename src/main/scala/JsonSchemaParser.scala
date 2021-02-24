package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri

import scala.collection.mutable

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
      multipleOf <- parsePositiveNumberOpt(obj, "multipleOf")
      max <- parseNumberOpt(obj, "maximum")
      exclMax <- parseNumberOpt(obj, "exclusiveMaximum")
      min <- parseNumberOpt(obj, "minimum")
      exclMin <- parseNumberOpt(obj, "exclusiveMinimum")
      maxLen <- parseNonNegativeIntegerOpt(obj, "maxLength")
      minLen <- parseNonNegativeIntegerWithDefaultZero(obj, "minLength")
      pattern <- parsePatternOpt(obj)
      items <- parseItems(obj)
      additionalItems <- parseSchemaOpt(obj, "additionalItems")
      maxItems <- parseNonNegativeIntegerOpt(obj, "maxItems")
      minItems <- parseNonNegativeIntegerWithDefaultZero(obj, "minItems")
      uniqueItems <- parseUniqueItems(obj)
      contains <- parseSchemaOpt(obj, "contains")
      maxProps <- parseNonNegativeIntegerOpt(obj, "maxProperties")
      minProps <- parseNonNegativeIntegerWithDefaultZero(obj, "minProperties")
      required <- parseRequired(obj)
      properties <- parseSchemaMap(obj, "properties")
      patternProps <- parsePatternProperties(obj)
      additionalProps <- parseSchemaOpt(obj, "additionalProperties")
      deps <- parseDependencies(obj)
      propNames <- parseSchemaOpt(obj, "propertyNames")
      const <- parseAnyOpt(obj, "const")
      types <- parseTypes(obj)
      enum <- parseEnum(obj)
      format <- parseStringOpt(obj, "format")
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
        additionalItems,
        maxItems,
        minItems,
        uniqueItems,
        contains,
        maxProps,
        minProps,
        required,
        properties,
        patternProps,
        additionalProps,
        deps,
        propNames,
        const,
        types,
        enum,
        format,
        allOf,
        anyOf,
        oneOf,
        not,
      )

  private def parseItems(value: ujson.Obj) = {
    val elemName = "items"
    val parser = (node: ujson.Value) => {
      for {
        items <- node match {
          case n: ujson.Obj => parseSubSchema(n).flatMap(schema => Right(Seq(schema)))
          case ujson.Arr(a) => parseSchemas(a, elemName)
          case _ => Left(ParserError(s"$elemName must be an object or array"))
        }
      } yield items
    }
    runSeqParser(value, elemName, parser)
  }

  private def parseDependencies(obj: ujson.Obj) = {
    val parser = (rawProps: mutable.AbstractMap[String, ujson.Value], elemName: String) => {
      rawProps.foldLeft(Right(Map[String, Either[Seq[String], JsonSchema]]()).withLeft[ParserError]) { case (acc, (k,v)) =>
        for {
          last <- acc
          result <- v match {
            case obj: ujson.Obj =>
              parseSchema(obj, "expected object")
                .map(s => Right(s))
            case ujson.Arr(arr) =>
              parseStringArray(arr, elemName)
                .map(a => Left(a))
            case _ => Left(ParserError(s"$elemName values must be an object or string array"))
          }
        } yield last + (k -> result)
      }
    }
    val elemName = "dependencies"
    runMapParser(obj, elemName, mapParser(parser)(elemName))
  }

  private def parseSchemaMap(obj: ujson.Obj, elemName: String): Either[ParserError, Map[String,JsonSchema]] = {
    val parser = (rawProps: mutable.AbstractMap[String, ujson.Value], elemName: String) => {
      rawProps.foldLeft(Right(Map[String,JsonSchema]()).withLeft[ParserError]) { case (acc, (k, v)) =>
        for {
          last <- acc
          schema <- parseSchema(v, s"$elemName values must be objects")
        } yield last + (k -> schema)
      }
    }
    runMapParser(obj, elemName, mapParser(parser)(elemName))
  }

  private def parseRequired(value: ujson.Obj)= {
    val elemName = "required"
    runSeqParser(value, elemName, arrayParser(parseStringArray)(elemName))
  }

  private def parseTypes(obj: ujson.Obj) = {
    val elemName = "type"
    val parser = (node: ujson.Value) => {
      for {
        types <- node match {
          case ujson.Str(s) => Right(Seq(s))
          case ujson.Arr(a) => parseStringArray(a, elemName)
          case _ => Left(ParserError(s"$elemName must be a string or array"))
        }
      } yield types
    }
    runSeqParser(obj, elemName, parser)
  }

  private def parseStringArray(items: IterableOnce[ujson.Value], elemName: String) = {
    for {
      elems <- items.iterator.foldLeft(Right(Seq[String]()).withLeft[ParserError]) { case (acc, cur) =>
        for {
          last <- acc
          elem <- parseString(cur, s"$elemName value")
        } yield last :+ elem
      }
    } yield elems
  }

  private def parseEnum(obj: ujson.Obj) = {
    val elemName = "enum"
    runSeqParser(obj, elemName, arrayParser((items,_) => Right(items.iterator.toSeq))(elemName))
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

  private def parseSchemaArray(value: ujson.Obj, elemName: String) =
    runSeqParser(value, elemName, arrayParser(parseSchemas)(elemName))

  private def parseSchemas(items: IterableOnce[ujson.Value], elemName: String) = {
    items.iterator.foldLeft(Right(Seq[JsonSchema]()).withLeft[ParserError]) { case (acc, cur) =>
      for {
        last <- acc
        schema <- parseSchema(cur, s"$elemName array contents must be objects")
      } yield last :+ schema
    }
  }

  private def parseSchemaOpt(obj: ujson.Obj, elemName: String) =
    runOptParser(obj, elemName, optParser(parseSchema))

  private def parseSchema(value: ujson.Value, errMsg: String) =
    for {
      obj <- value.objOpt.toRight(ParserError(errMsg))
      schema <- parseSubSchema(obj)
    } yield schema

  private def parsePatternProperties(obj: ujson.Obj) =
    //TODO: verify keys are ECMA 262 regexes
    parseSchemaMap(obj, "patternProperties")

  private def parsePatternOpt(obj: ujson.Obj) =
    //TODO: verify value is a ECMA 262 regex
    parseStringOpt(obj, "pattern")

  private def parseNonNegativeIntegerWithDefault(default: Int)(obj: ujson.Obj, elemName: String) =
    for {
      num <- parseNonNegativeIntegerOpt(obj, elemName)
      result = num.getOrElse(default)
    } yield result

  private def parseNonNegativeIntegerWithDefaultZero =
    parseNonNegativeIntegerWithDefault(0) _

  private def parseNonNegativeIntegerOpt(obj: ujson.Obj, elemName: String) =
    runOptParser(obj, elemName, optParser(parseNonNegativeInteger))

  private def parsePositiveNumberOpt(obj: ujson.Obj, elemName: String) =
    runOptParser(obj, elemName, optParser(parsePositiveNumber))

  private def parseNumberOpt(obj: ujson.Obj, elemName: String) =
    runOptParser(obj, elemName, optParser(parseNumber))

  private def parseBoolOpt(value: ujson.Obj, elemName: String) =
    runOptParser(value, elemName, optParser(parseBool))

  private def parseUriOpt(value: ujson.Obj, elemName: String): Either[ParserError, Option[Uri]] =
    runOptParser(value, elemName, optParser(parseUri))

  private def parseStringOpt(value: ujson.Obj, elemName: String) =
    runOptParser(value, elemName, optParser(parseString))

  private def parseAnyOpt(value: ujson.Obj, elemName: String) =
    runOptParser(value, elemName, optParser(parseIdentity))

  //TODO: DRY up number parsing with validation
  private def parseNonNegativeInteger(value: ujson.Value, elemName: String) =
    for {
      num <- parseInteger(value, elemName)
      result <-
        if (num < 0)
          Left(ParserError(s"$elemName must be >= 0"))
        else
          Right(num)
    } yield result

  private def parseInteger(value: ujson.Value, elemName: String) =
    parseNumber(value, elemName).map(_.toInt)

  private def parsePositiveNumber(value: ujson.Value, elemName: String) =
    for {
      num<- parseNumber(value, elemName)
      result <-
        if (num <= 0)
          Left(ParserError(s"$elemName must be > 0"))
        else
          Right(num)
    } yield result

  private def parseNumber(value: ujson.Value, elemName: String) =
    value.numOpt.toRight(ParserError(s"$elemName must be a number"))

  private def parseBool(value: ujson.Value, elemName: String) =
    value.boolOpt.toRight(ParserError(s"$elemName must be a boolean"))

  private def parseUri(value: ujson.Value, elemName: String) =
    for {
      uriStr <- parseString(value, elemName)
      uri <- Uri.parseOption(uriStr).toRight(ParserError(s"Invalid $elemName URI"))
    } yield uri

  private def parseString(value: ujson.Value, elemName: String) =
    value.strOpt.toRight(ParserError(s"$elemName must be a string"))

  //noinspection ScalaUnusedSymbol
  private def parseIdentity(value: ujson.Value, elemName: String) =
    Right(value).withLeft[ParserError]

  private def arrayParser[T]
    (parser: (IterableOnce[ujson.Value],String) => Either[ParserError, Seq[T]])
    (elemName: String)
    (node: ujson.Value) =
      for {
        items <- node.arrOpt.toRight(ParserError(s"$elemName must be an array"))
        result <- parser(items, elemName)
      } yield result

  private def mapParser[T]
    (parser: (mutable.AbstractMap[String, ujson.Value],String) => Either[ParserError, Map[String, T]])
    (elemName: String)
    (node: ujson.Value) =
      for {
        rawProps <- node.objOpt.toRight(ParserError(s"$elemName must be an object"))
        props <- parser(rawProps, elemName)
      } yield props

  private def optParser[T]
    (parser: (ujson.Value,String) => Either[ParserError,T])
    (elemName: String)
    (node: ujson.Value) =
      for {
        result <- parser(node, elemName)
      } yield Option(result)

  //TODO: Figure out how to make a function that's generic over several containers
  /** Checks for the existence of an element before running the parser */
  private def runOptParser[T](value: ujson.Obj, elemName: String, parser: String => ujson.Value => Either[ParserError, Option[T]]) =
    if (value.obj.keys.exists(k => k == elemName)) {
      val node = value(elemName)
      parser(elemName)(node)
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
