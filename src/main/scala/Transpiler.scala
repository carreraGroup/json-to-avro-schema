package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri

object Transpiler {
  /*
   * There could be many passes, but this signature should hide them
   * JsonSchema -> AvroSchema
   * Internally, we may do this though
   * JsonSchema -> A -> B -> C -> AvroRecord
   */
  def transpile(schema: JsonSchema, namespace: Option[String]): Either[TranspileError, AvroRecord] = {
    for {
      fields <- resolveFields(schema)
      name = schema.id.map(toName).getOrElse("unknown")
    } yield AvroRecord(name, namespace, schema.desc, fields)
  }

  private def toName(id: Uri) =
    id.path.parts.last

  private def resolveFields(schema: JsonSchema) =
    schema.properties.foldLeft(Right(Seq[AvroField]()).withLeft[TranspileError]) { case (acc, (name, prop)) =>
      for {
        last <- acc
        t <- resolveType(name, prop)
        (avroType, default) =
          if (schema.required.contains(name))
            (t, None)
          else
            (AvroUnion(Seq(AvroNull, t)), Some(ujson.Null))

        field = AvroField(name, prop.desc, avroType, default, None /* TODO: order */)
      } yield last :+ field
    }

  private def resolveType(propName: String, schema: JsonSchema): Either[TranspileError, AvroType] = {
    schema.types match {
      case Nil =>
        /* types and enum aren't really mutually exclusive, but having both makes no sense in avro */
        schema.`enum` match {
          case Nil => Right(AvroBytes)
          case xs => resolveEnum(propName, xs)
        }
      case x :: Nil => resolveType(propName, schema, x)
      case xs => xs.foldLeft(Right(Seq[AvroType]()).withLeft[TranspileError]) { case (acc, cur) =>
        for {
          last <- acc
          t <- resolveType(propName, schema, cur)
        } yield last :+ t
      }.map(types => AvroUnion(types))
    }
  }

  private def resolveType(propName: String, schema: JsonSchema, jsonSchemaType: JsonSchemaType): Either[TranspileError, AvroType] =
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
              itemType <- resolveType(propName, x)
            } yield AvroArray(itemType)
          case x :: xs => Left(TranspileError(s"Unimplemented: index by index array validation isn't supported yet at $propName"))
        }
      case JsonSchemaObject =>
        schema.id match {
          //how might we simply call transpile and be done?
          case Some(_) => transpile(schema, None)
          case None =>
            //TODO: additional props only apply to props not present in properties
            // they're accumulative, not exclusive
            schema.additionalProperties match {
              case None => Left(TranspileError(s"object without a type at $propName"))
              case Some(additionalProps) => for {
                valueType <- resolveType(propName, additionalProps)
              } yield AvroMap(valueType)
            }
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
}

final case class TranspileError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
