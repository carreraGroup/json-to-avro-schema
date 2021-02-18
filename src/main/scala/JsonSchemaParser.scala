package io.carrera.jsontoavroschema
import io.lemonlabs.uri.Url

import scala.util.Try

case class JsonSchema(schemaId: Url)

object JsonSchemaParser {

  def parse(value: ujson.Value): Either[Throwable, JsonSchema] =
    for {
      // The schema *should* be used to determine how to parse the rest of the document
      // For now, we are just assuming it's a draft 6 document
      schemaUri <- parseSchemaUri(value)
    } yield JsonSchema(schemaUri)

  def parseSchemaUri(value: ujson.Value) = {
    for {
      schemaNode <-
        Try(value("$schema"))
          .toEither
          .left
          .map(err => ParserError("$schema must be specified", err))
      uriStr <-schemaNode.strOpt.toRight(ParserError("No URI exists for $schema"))
      uri <- Url.parseOption(uriStr).toRight(ParserError("Invalid schema URI"))
      //TODO: The spec says the schema uri must include a scheme. Validate it does.
      // https://tools.ietf.org/html/draft-wright-json-schema-01#section-7
    } yield uri
  }
}

final case class ParserError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
