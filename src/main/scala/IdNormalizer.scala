package io.carrera.jsontoavroschema

import Json._

import io.lemonlabs.uri._

object IdNormalizer {
  def normalizeIds(root: JSchema): Either[ResolutionError, JSchema] = {
    root match {
      case Left(bool) => Right(Left(bool))
      case Right(schema) =>
        for {
          baseUri <- schema.id.toRight(ResolutionError("$id must be specified in root schema"))
          resolved <- normalizeIds(Right(schema), baseUri)
        } yield resolved
    }
  }

  private def normalizeIds(schema: JSchema, baseUri: Uri): Either[ResolutionError, JSchema] = {
    schema match {
      case Left(bool) => Right(Left(bool))
      case Right(schema) =>
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
        } yield Right(schema.copy(
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
        ))
    }
  }

  private def resolveDependencies(deps: Map[String, Either[Seq[String], JSchema]], baseUri: Uri): Either[ResolutionError,  Map[String, Either[Seq[String], JSchema]]] =
    deps.foldLeft(Right(Map[String, Either[Seq[String], JSchema]]()).withLeft[ResolutionError]) { case (acc, (k, v)) =>
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

  private def resolveSchemas(schemas: IterableOnce[JSchema], baseUri: Uri) =
    schemas.iterator.foldLeft(Right(Seq[JSchema]()).withLeft[ResolutionError]) { case (acc, cur) =>
      for {
        last <- acc
        schema <- resolveSchema(cur, baseUri)
      } yield last :+ schema
    }

  private def resolveSchemas(schemaMap: Map[String, JSchema], baseUri: Uri) =
    schemaMap.foldLeft(Right(Map[String, JSchema]()).withLeft[ResolutionError]) { case (acc, (k, v)) =>
      for {
        last <- acc
        schema <- resolveSchema(v, baseUri)
      } yield last + (k -> schema)
    }

  private def resolveSchema(maybeSchema: Option[JSchema], baseUri: Uri): Either[ResolutionError, Option[JSchema]] =
    maybeSchema match {
      case None => Right(None)
      case Some(schema) =>
        resolveSchema(schema, baseUri).map(Some(_))
    }

  private def resolveSchema(schema: JSchema, baseUri: Uri): Either[ResolutionError, JSchema] = {
    schema match {
      case Left(bool) => Right(Left(bool))
      case Right(schema) =>
        for {
          id <- resolveId(schema.id, baseUri)
          cur = schema.copy(id = id)
          newBaseUri = id.getOrElse(baseUri)
          resolved <- normalizeIds(Right(cur), newBaseUri)
        } yield resolved
    }
  }

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