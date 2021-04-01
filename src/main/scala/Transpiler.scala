package io.carrera.jsontoavroschema

import SymbolResolver.{Symbols, resolve}

import io.lemonlabs.uri.{AbsoluteUrl, RelativeUrl, Uri}

import scala.util.chaining.scalaUtilChainingOps

case class Context(parent: JsonSchema, namespace: Option[String], symbols: Symbols)

object Transpiler {
  /*
   * There could be many passes, but this signature should hide them
   * JsonSchema -> AvroSchema
   * Internally, we may do this though
   * JsonSchema -> A -> B -> C -> AvroRecord
   */
  def transpile(schema: JsonSchema, namespace: Option[String]): Either[TranspileError, AvroRecord] = {
    for {
      name <- schema.id.map(toName).toRight(TranspileError("$id must be specified in root schema"))
      normalized <- IdNormalizer.normalizeIds(schema).left.map(err => TranspileError("Failed to normalize IDs", err))
      symbols = SymbolResolver.resolve(normalized)
      ctx = Context(normalized, namespace, symbols)
      record <- transpile(name, ctx)
    } yield record
  }

  private def transpile(name: String, ctx: Context): Either[TranspileError, AvroRecord] = {
    for {
      fields <- resolveFields(ctx.parent.properties, ctx)
      defs <- resolveDefinitions(ctx.parent.definitions, ctx)
      resolvedFields =
        defs.foldLeft(fields)(replaceFirstReferenceToDefinition)
    } yield AvroRecord(name, ctx.namespace, ctx.parent.desc, resolvedFields)
  }

  private def replaceFirstReferenceToDefinition(fields: Seq[AvroField], definition: AvroRecord) = {
    val idx = fields.indexWhere(f => f.`type` match {
      case AvroRef(name) => name == definition.name
      case _ => false
    })
    fields.updated(idx, fields(idx).copy(`type` = definition))
  }

  private def toName(id: Uri) =
    id.path.parts.last

  private def resolveFields(schemas: Map[String, JsonSchema], ctx: Context):Either[TranspileError, Seq[AvroField]] =
    schemas.foldLeft(Right(Seq[AvroField]()).withLeft[TranspileError]) { case (acc, (name, prop)) =>
        for {
          last <- acc
          field <- resolveField(name, prop, ctx)
        } yield last :+ field
    }

  private def resolveDefinitions(defs: Map[String, JsonSchema], ctx: Context): Either[TranspileError, Seq[AvroRecord]] =
    defs.foldLeft(Right(Seq[AvroRecord]()).withLeft[TranspileError]) { case (acc, (name, definition)) =>
      for {
        last <- acc
        t <- resolveType(name, definition, ctx)
        record = AvroRecord(
          name,
          None,
          definition.desc,
          Seq(AvroField("value", None, t, None, None))
        )
      } yield last :+ record
    }

  private def resolveField(name: String, prop: JsonSchema, ctx: Context) =
    for {
      typeAndDefault <-
        if (prop.properties.isEmpty)
          for {
            t <- resolveType(name, prop, ctx)
            (avroType, default) =
            if (ctx.parent.required.contains(name))
              (t, None)
            else
              (AvroUnion(Seq(AvroNull, t)), Some(ujson.Null))
          } yield (avroType, default)
        else {
          val subCtx = Context(prop, None, ctx.symbols)
          for {
            record <- transpile(prop.id.map(toName).getOrElse(name), subCtx)
          } yield (record, None)
        }
    } yield AvroField(name, prop.desc, typeAndDefault._1, typeAndDefault._2, None /* TODO: order */)

  private def resolveType(propName: String, schema: JsonSchema, ctx: Context): Either[TranspileError, AvroType] = {
    /*
     * according to the spec, all other properties MUST be ignored if a ref is present
     * https://tools.ietf.org/html/draft-wright-json-schema-01#section-8
     */
    schema.ref match {
      case Some(uri) => resolveRefUri(uri, ctx.symbols)
      case None =>
        schema.types match {
          case Nil =>
            /* types and enum aren't really mutually exclusive, but having both makes no sense in avro */
            schema.`enum` match {
              case Nil => Right(AvroBytes)
              case xs => resolveEnum(propName, xs)
            }
          case x :: Nil => resolveType(propName, schema, x, ctx)
          case xs => xs.foldLeft(Right(Seq[AvroType]()).withLeft[TranspileError]) { case (acc, cur) =>
            for {
              last <- acc
              t <- resolveType(propName, schema, cur, ctx)
            } yield last :+ t
          }.map(types => AvroUnion(types))
        }
    }
  }

  private def resolveType(propName: String, schema: JsonSchema, jsonSchemaType: JsonSchemaType, ctx: Context): Either[TranspileError, AvroType] =
    jsonSchemaType match {
      case JsonSchemaString => Right(AvroString)
      case JsonSchemaNumber => Right(AvroDouble)
      case JsonSchemaBool => Right(AvroBool)
      case JsonSchemaNull => Right(AvroNull)
      case JsonSchemaInteger => Right(AvroLong)
      case JsonSchemaArray =>
        schema.items match {
          case Nil => Right(AvroArray(AvroBytes))
          case x :: Nil =>
            for {
              itemType <- resolveType(propName, x, ctx)
            } yield AvroArray(itemType)
          case x :: xs => Left(TranspileError(s"Unimplemented: index by index array validation isn't supported yet at $propName"))
        }
      case JsonSchemaObject =>
        schema.additionalProperties match {
          case None => Left(TranspileError(s"object without a type at $propName"))
          case Some(additionalProps) => for {
            valueType <- resolveType(propName, additionalProps, ctx)
          } yield AvroMap(valueType)
        }
    }

  private def resolveEnum(propName: String, value: Seq[ujson.Value]): Either[TranspileError, AvroEnum] =
    value.foldLeft(Right(Seq[String]()).withLeft[TranspileError]) { case (acc, cur) =>
      for {
        last <- acc
        str <- cur match {
          case ujson.Str(s) => Right(s)
          case v => Left(TranspileError(s"Unimplemented: non-string enums aren't supported yet at $propName. Value: $v"))
        }
      } yield last :+ str
    } /* json schema enums don't have names, so we build one from it's parent property */
      .map(values => AvroEnum(s"${propName}Enum", values))

  private def resolveRefUri(uri: Uri, symbols: Symbols): Either[TranspileError, AvroRef] = {
    symbols.getOrElse(uri, uri) match {
      case RelativeUrl(_, _, fragment) =>
        fragment
          .toRight(TranspileError("Expected Uri fragment in ref Uri"))
          .map(fragment => fragment.split("/").last)
          .map(AvroRef)
      case AbsoluteUrl(_,_,path,_,_) =>
        path.parts.last.pipe(AvroRef).pipe(Right.apply)
      case unknown => Left(TranspileError(s"Unimplemented ref URI type for: $unknown"))
    }
  }
}
