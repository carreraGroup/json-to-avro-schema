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
    schema.properties.foldLeft(Right(Seq[AvroField]()).withLeft[TranspileError]) { case (acc, (k, v)) =>
      for {
        last <- acc
        t <- resolveType(v)
        field =
        AvroField(k, v.desc, t,
          None, //TODO: default
          None //TODO: order
        )
      } yield last :+ field
    }

  private def resolveType(schema: JsonSchema): Either[TranspileError, AvroType] = {
    schema.types match {
      case Nil => Right(AvroBytes)
      case value :: Nil =>
        value match {
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
                  itemType <- resolveType(x)
                } yield AvroArray(itemType)
              case x :: xs => Left(TranspileError("Unimplemented: array items must have a single type"))
            }
          case JsonSchemaObject =>
            schema.id match {
              //how might we simply call transpile and be done?
              case Some(_) => transpile(schema, None)
              case None =>
                //TODO: additional props only apply to props not present in properties
                // they're accumulative, not exclusive
                schema.additionalProperties match {
                  case None => Left(TranspileError(s"object without a type"))
                  case Some(additionalProps) => for {
                    valueType <- resolveType(additionalProps)
                  } yield AvroMap(valueType)
                }
            }
        }
      case x :: xs => Left(TranspileError("Unimplemented: unions aren't supported yet"))
    }
  }
}

final case class TranspileError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
