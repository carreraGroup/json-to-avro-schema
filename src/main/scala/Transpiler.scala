package io.carrera.jsontoavroschema

import AvroOrder._

//TODO: do something better than Any for default
case class AvroField(name: String, doc: Option[String], `type`: AvroType, default: Any, order: Option[AvroOrder])
case class AvroRecord(name: String, namespace: Option[String], doc: Option[String], fields: Seq[AvroField])

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
    } yield AvroRecord("schema", namespace, schema.desc, fields)
  }

  private def resolveFields(schema: JsonSchema) =
    schema.properties.foldLeft(Right(Seq[AvroField]()).withLeft[TranspileError]) { case (acc, (k,v)) =>
      for {
        last <- acc
        t <- resolveType(v)
        field =
          AvroField(k, v.desc, t,
            None, //TODO: default
            None  //TODO: order
          )
      } yield last :+ field
    }

  private def resolveType(schema: JsonSchema) =
    AvroType
      .fromJsonSchema(schema.types, schema.items)
      .left
      .map(msg => TranspileError(msg))
}

final case class TranspileError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
