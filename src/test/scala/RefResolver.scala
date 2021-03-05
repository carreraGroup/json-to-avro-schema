package io.carrera.jsontoavroschema

import io.lemonlabs.uri.typesafe.dsl._
import io.lemonlabs.uri.{AbsoluteUrl, RelativeUrl, Uri}

object RefResolver {
  def normalizeIds(root: JsonSchema): Either[ResolutionError, JsonSchema] =
    for {
      baseUri <- root.id.toRight(ResolutionError("$id must be specified in root schema"))
      resolved <- normalizeIds(root, baseUri)
    } yield resolved

  private def normalizeIds(schema: JsonSchema, baseUri: Uri): Either[ResolutionError, JsonSchema] =
    for {
      definitions <- resolveDefinitions(schema.definitions, baseUri, schema)
    } yield schema.copy(definitions = definitions)

  private def resolveDefinitions(definitions: Map[String, JsonSchema], baseUri: Uri, ctx: JsonSchema) =
    definitions.foldLeft(Right(Map[String, JsonSchema]()).withLeft[ResolutionError]) { case (acc, (k, v)) =>
      for {
        last <- acc
        definition <- resolveDefinition(v, baseUri, ctx)
      } yield last + (k -> definition)
    }

  private def resolveDefinition(schema: JsonSchema, baseUri: Uri, ctx: JsonSchema): Either[ResolutionError, JsonSchema] =
    for {
      id <- resolveId(schema.id, baseUri)
      cur = schema.copy(id = id)
      resolved <- normalizeIds(cur, id.getOrElse(baseUri))
    } yield resolved

  private def resolveId(maybeId: Option[Uri], baseUri: Uri): Either[ResolutionError, Option[Uri]] =
    maybeId match {
      case None => Right(None)
      case Some(id) =>
        for {
          absoluteId <- combineUris(baseUri, id)
        } yield Some(absoluteId)
    }

  private def combineUris(baseUri: Uri, id: Uri) =
    (baseUri, id) match {
      case (base: AbsoluteUrl, rel: RelativeUrl) => Right(base / rel)
      case unknown => Left(ResolutionError(s"Unimplemented URI combination: $unknown"))
    }
}

final case class ResolutionError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)