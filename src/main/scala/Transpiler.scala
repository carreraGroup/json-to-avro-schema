package io.carrera.jsontoavroschema

import SymbolResolver.Symbols
import Thing.JSchema

import io.lemonlabs.uri.{AbsoluteUrl, RelativeUrl, Uri}

import scala.util.chaining.scalaUtilChainingOps

case class Context(parent: JSchema, namespace: Option[String], symbols: Symbols)

object Transpiler {
  /*
   * There could be many passes, but this signature should hide them
   * JsonSchema -> AvroSchema
   * Internally, we may do this though
   * JsonSchema -> A -> B -> C -> AvroRecord
   */
  def transpile(schema: JSchema, namespace: Option[String]): Either[TranspileError, AvroRecord] = {
    schema match {
      case Left(_) => Left(TranspileError("root schema must be a schema, not a boolean"))
      case Right(schema) =>
        for {
          name <- schema.id.map(toName).toRight(TranspileError("$id must be specified in root schema"))
          normalized <- IdNormalizer.normalizeIds(Right(schema)).left.map(err => TranspileError("Failed to normalize IDs", err))
          symbols = SymbolResolver.resolve(normalized)
          ctx = Context(normalized, namespace, symbols)
          record <- transpile(name, ctx)
        } yield record
    }
  }

  private def transpile(name: String, ctx: Context): Either[TranspileError, AvroRecord] = {
    ctx.parent match {
      case Left(_) => Left(TranspileError(s"Unexpected boolean schema at $name"))
      case Right(schema) =>
        for {
          fields <- resolveFields(ctx)
          defs <- resolveDefinitions(schema.definitions, ctx)
          resolvedFields =
          defs.foldLeft(fields)(replaceFirstReferenceToDefinition)
        } yield AvroRecord(name, ctx.namespace, schema.desc, resolvedFields)
    }
  }

  private def replaceFirstReferenceToDefinition(fields: Seq[AvroField], definition: AvroRecord) = {
    def isDefRef: PartialFunction[AvroType, Boolean] = {
      case AvroRef(name) => name == definition.name
      case AvroUnion(types) => types.exists(isDefRef)
      case _ => false
    }

    val idx = fields.indexWhere(f => isDefRef(f.`type`))

    // this allows for unreferenced definitions
    if (idx == -1)
      fields
    else {
      val field = fields(idx)
      val resolvedType =
        field.`type` match {
          case AvroUnion(types) => AvroUnion(definition +: types.filterNot(isDefRef))
          case _ => definition
        }
      fields.updated(idx, field.copy(`type` = resolvedType))
    }
  }

  private def toName(id: Uri) =
    id.path.parts.last

  private def resolveFields(ctx: Context):Either[TranspileError, Seq[AvroField]] =
    for {
      props <- resolveProperties(ctx)
      oneOf <- resolveOneOfField(ctx)
    } yield props ++ oneOf

  private def resolveProperties(ctx: Context) = {
    ctx.parent match {
      case Left(_) => Left(TranspileError("root schema cannot be a boolean schema"))
      case Right(schema) =>
        schema.properties.foldLeft(Right(Seq[AvroField] ()).withLeft[TranspileError]) { case (acc, (name, prop)) =>
          for {
            last <- acc
            field <- resolveField(name, prop, ctx)
          } yield last :+ field
        }
    }
  }

  private def resolveOneOfField(ctx: Context): Either[TranspileError, Seq[AvroField]] = {
    ctx.parent match {
      case Left(_) => Left(TranspileError("root schema cannot be a boolean schema"))
      case Right(schema) => {
        val schemas = schema.oneOf
        if (schemas.isEmpty)
          Right(Seq())
        else
          for {
            union <- resolveOneOf("oneOf", schemas, ctx)
          } yield Seq(AvroField("value", schema.desc, union, None, None))
      }
    }
  }

  private def resolveDefinitions(defs: Map[String, JSchema], ctx: Context): Either[TranspileError, Seq[AvroRecord]] = {
    defs.foldLeft(Right(Seq[AvroRecord]()).withLeft[TranspileError]) { case (acc, (name, definition)) =>
      definition match {
        case Left(_) => Left(TranspileError("definition cannot be a boolean schema"))
        case Right(schema) =>
          for {
            last <- acc
            subCtx = Context(definition, None, ctx.symbols)
            // if we have properties, don't wrap the record in another record
            fields <-
              if (schema.properties.nonEmpty)
                for {
                  fs <- resolveFields(subCtx)
                } yield fs
              else {
                for {
                  t <- resolveType(name, Right(schema), subCtx)
                } yield Seq(AvroField("value", None, t, None, None))
              }
            record = AvroRecord(name, None, schema.desc, fields)
          } yield last :+ record
      }
    }
  }

  private def resolveField(name: String, prop: JSchema, ctx: Context) = {
    prop match {
      case Left(_) => Left(TranspileError(s"Unexpected boolean schema at $name"))
      case Right(schema) =>
        for {
          typeAndDefault <-
            if (schema.properties.isEmpty)
              for {
                t <- resolveType(name, Right(schema), ctx)
                (avroType, default) =
                  ctx.parent match {
                    case Left(_) => throw TranspileError(s"Unexpected boolean schema parent of $name. This should be impossible.")
                    case Right(parent) =>
                      if (parent.required.contains(name))
                        (t, None)
                      else
                        (AvroUnion(Seq(AvroNull, t)).flatten, Some(ujson.Null))
                  }
              } yield (avroType, default)
            else {
              val subCtx = Context(prop, None, ctx.symbols)
              for {
                record <- transpile(schema.id.map(toName).getOrElse(name), subCtx)
              } yield (record, None)
            }
        } yield AvroField(name, schema.desc, typeAndDefault._1, typeAndDefault._2, None /* TODO: order */)
    }
  }

  private def resolveType(propName: String, schema: JSchema, ctx: Context): Either[TranspileError, AvroType] = {
    def optionalList[T](xs: Seq[T]): Option[Seq[T]] =
      if (xs.isEmpty) None else Some(xs)

    /*
     * according to the spec, all other properties MUST be ignored if a ref is present
     * https://tools.ietf.org/html/draft-wright-json-schema-01#section-8
     */
    schema match {
      case Left(bool) =>
        if (bool)
          Right(AvroBytes)
        else
          Left(TranspileError(s"A false value in $propName ensures there are no valid schemas"))
      case Right(schema) =>
        schema.ref match {
          case Some(uri) => resolveRefUri(uri, ctx.symbols)
          case None =>
            /*
             types, enum, oneOf, etc. are not actually mutually exclusive.
             https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-4.4
             However, JsonSchema also allows you to create schemas that don't make any sense
             by combining these things together.
             We treat them as mutually exclusive for simplicity.
             */
            optionalList(schema.types) map {
              case x :: Nil => resolveType(propName, Right(schema), x, ctx)
              case xs => xs.foldLeft(Right(Seq[AvroType]()).withLeft[TranspileError]) { case (acc, cur) =>
                for {
                  last <- acc
                  t <- resolveType(propName, Right(schema), cur, ctx)
                } yield last :+ t
              }.map(AvroUnion)
            } orElse {
              optionalList(schema.`enum`)
                .map(resolveEnum(propName, _))
            } orElse {
              optionalList(schema.oneOf)
                .map(resolveOneOf(propName, _, ctx))
            } getOrElse {
              Right(AvroBytes)
            }
        }
    }
  }

  private def resolveType(propName: String, schema: JSchema, jsonSchemaType: JsonSchemaType, ctx: Context): Either[TranspileError, AvroType] = {
    schema match {
      case Left(_) => Left(TranspileError(s"Unexpected boolean schema at $propName"))
      case Right(schema) =>
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
    }

  }

  private def resolveOneOf(propName: String, schemas: Seq[JSchema], ctx: Context): Either[TranspileError, AvroUnion] = {
      schemas.foldLeft(Right(Seq[AvroType]()).withLeft[TranspileError]) { case (acc, cur) =>
        for {
          last <- acc
          t <- resolveType(propName, cur, ctx)
        } yield last :+ t
      }.map(AvroUnion)
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
