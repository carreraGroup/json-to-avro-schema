package io.carrera.jsontoavroschema

import AvroType._
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
    Right(AvroRecord("schema", namespace, schema.desc, resolveFields(schema).toSeq))
  }

  private def resolveFields(schema: JsonSchema) =
    schema.properties.map { case (k,v) =>
      AvroField(
        k,
        v.desc,
        resolveType(v),
        None, //TODO: default
        None  //TODO: order
      )
    }

  private def resolveType(schema: JsonSchema) =
    fromJsonSchema(schema.types.head)
}

final case class TranspileError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
