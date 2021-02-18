package io.carrera.jsontoavroschema
import io.lemonlabs.uri.Url
import ujson.Value

import scala.util.Try

case class JsonSchema(schemaUri: Option[Url])

object JsonSchemaParser {

  def parse(value: ujson.Value): Either[Throwable, JsonSchema] =
    for {
      // The schema *should* be used to determine how to parse the rest of the document
      // For now, we are just assuming it's a draft 6 document
      schemaUri <- parseSchemaUri(value)
    } yield JsonSchema(schemaUri)

  def parseSchemaUri(value: ujson.Value): Either[ParserError, Option[Url]] = {
    Try(value("$schema")).toOption match {
      case Some(node) => {
        for {
          uriStr <- node.strOpt.toRight(ParserError("$schema must be a URI string"))
          uri <- Url.parseOption(uriStr).toRight(ParserError("Invalid $schema URI"))
          //TODO: The spec says the schema uri must include a scheme. Validate it does.
          // https://tools.ietf.org/html/draft-wright-json-schema-01#section-7
        } yield Some(uri)
      }
      case None => Right(None)
    }
  }
}

final case class ParserError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
