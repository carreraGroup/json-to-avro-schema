package io.carrera.jsontoavroschema

import io.lemonlabs.uri.{AbsoluteUrl, EmptyPath, RelativeUrl, RootlessPath, Uri, Urn}

object IdNormalizer {
  def normalizeIds(root: JsonSchema): Either[ResolutionError, JsonSchema] =
    for {
      baseUri <- root.id.toRight(ResolutionError("$id must be specified in root schema"))
      resolved <- normalizeIds(root, baseUri)
    } yield resolved

  private def normalizeIds(schema: JsonSchema, baseUri: Uri): Either[ResolutionError, JsonSchema] =
    for {
      definitions <- resolveSchemas(schema.definitions, baseUri)
      additionalItems <- resolveSchema(schema.additionalItems, baseUri)
      contains <- resolveSchema(schema.contains, baseUri)
      properties <- resolveSchemas(schema.properties, baseUri)
      patternProps <- resolveSchemas(schema.patternProperties, baseUri)
      additionalProps <- resolveSchema(schema.additionalProperties, baseUri)
      deps <- resolveDependencies(schema.dependencies, baseUri)
      propNames <- resolveSchema(schema.propertyNames, baseUri)
      allOf <- resolveSchemas(schema.allOf, baseUri)
      anyOf <- resolveSchemas(schema.anyOf, baseUri)
      oneOf <- resolveSchemas(schema.oneOf, baseUri)
      not <- resolveSchema(schema.not, baseUri)
    } yield schema.copy(
      definitions = definitions,
      additionalItems = additionalItems,
      contains = contains,
      properties = properties,
      patternProperties = patternProps,
      additionalProperties = additionalProps,
      dependencies = deps,
      propertyNames = propNames,
      allOf = allOf,
      anyOf = anyOf,
      oneOf = oneOf,
      not = not,
    )

  private def resolveDependencies(deps: Map[String, Either[Seq[String], JsonSchema]], baseUri: Uri): Either[ResolutionError,  Map[String, Either[Seq[String], JsonSchema]]] =
    deps.foldLeft(Right(Map[String, Either[Seq[String], JsonSchema]]()).withLeft[ResolutionError]) { case (acc, (k, v)) =>
      for {
        last <- acc
        dep <- v match {
          case Left(propNames) => Right(Left(propNames))
          case Right(schema) => for {
            resolved <- resolveSchema(schema, baseUri)
          } yield Right(resolved)
        }
      } yield last + (k -> dep)
    }

  private def resolveSchemas(schemas: IterableOnce[JsonSchema], baseUri: Uri) =
    schemas.iterator.foldLeft(Right(Seq[JsonSchema]()).withLeft[ResolutionError]) { case (acc, cur) =>
      for {
        last <- acc
        schema <- resolveSchema(cur, baseUri)
      } yield last :+ schema
    }

  private def resolveSchemas(schemaMap: Map[String, JsonSchema], baseUri: Uri) =
    schemaMap.foldLeft(Right(Map[String, JsonSchema]()).withLeft[ResolutionError]) { case (acc, (k, v)) =>
      for {
        last <- acc
        schema <- resolveSchema(v, baseUri)
      } yield last + (k -> schema)
    }

  private def resolveSchema(maybeSchema: Option[JsonSchema], baseUri: Uri): Either[ResolutionError, Option[JsonSchema]] =
    maybeSchema match {
      case None => Right(None)
      case Some(schema) =>
        resolveSchema(schema, baseUri).map(Some(_))
    }

  private def resolveSchema(schema: JsonSchema, baseUri: Uri): Either[ResolutionError, JsonSchema] =
    for {
      id <- resolveId(schema.id, baseUri)
      cur = schema.copy(id = id)
      newBaseUri = id.getOrElse(baseUri)
      resolved <- normalizeIds(cur, newBaseUri)
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
      case(base: AbsoluteUrl, RelativeUrl(path, _, fragment)) =>
        path match {
          case EmptyPath() => Right(base.withFragment(fragment))
          // withPath replaces the path and makes it relative to the authority
          case path: RootlessPath => Right(base.withPath(path).withFragment(fragment))
          case unknown => Left(ResolutionError(s"Unimplemented URI path: $unknown"))
        }
      case(_, urn: Urn) => Right(urn)
      case(_, url: AbsoluteUrl) => Right(url)
      case unknown => Left(ResolutionError(s"Unimplemented URI combination: $unknown"))
    }
}

final case class ResolutionError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)