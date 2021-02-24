package io.carrera.jsontoavroschema

sealed trait AvroType
case class AvroString() extends AvroType
//TODO: add types for all the primitive and complex types

object AvroOrder extends Enumeration {
  type AvroOrder = Value
  val Ascending, Descending, Ignore = Value
}
import AvroOrder._

case class AvroField(name: String, doc: Option[String], `type`: AvroType, default: Any, order: Option[AvroOrder]) //TODO: do something better than Any
case class AvroRecord(name: String, namespace: Option[String], doc: Option[String], fields: Seq[AvroField])
case class AvroSchema(namespace: Option[String], record: AvroRecord)

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
    schema.types.head match {
      case "string" => AvroString()
    }
}

final case class TranspileError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
