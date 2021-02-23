package io.carrera.jsontoavroschema

sealed trait AvroType
//TODO: add types for all the primitive and complex types

object AvroOrder extends Enumeration {
  type AvroOrder = Value
  val Ascending, Descending, Ignore = Value
}
import AvroOrder._

case class AvroField(name: String, doc: Option[String], `type`: AvroType, default: Any, order: Option[AvroOrder]) //TODO: do something better than Any
case class AvroRecord(name: String, doc: Option[String], fields: Seq[AvroField])
case class AvroSchema(namespace: Option[String], records: Seq[AvroRecord])

object AvroWriter {
  def toJson(schema: AvroSchema): Either[TranspileError, ujson.Obj] =
    //TODO: implement avro schema to json conversion
    Right(ujson.Obj())
}

object Transpiler {
  /*
   * There could be many passes, but this signature should hide them
   * JsonSchema -> AvroSchema
   * Internally, we may do this though
   * JsonSchema -> A -> B -> C -> AvroSchema
   */
  def transpile(schema: JsonSchema): Either[TranspileError, AvroSchema] = {
    Right(AvroSchema(None, Seq()))
  }

  def transpile(schema: JsonSchema, namespace: String): Either[TranspileError, AvroSchema] = {
    Right(AvroSchema(Some(namespace), Seq()))
  }

  /*
  def toJson(avroSchema: AvroRecord): ujson.Obj = ???
   */
}

final case class TranspileError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
