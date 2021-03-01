package io.carrera.jsontoavroschema

import AvroOrder.AvroOrder

sealed trait AvroType {
  def serialize(): String =
    this match {
      case AvroString => "string"
      case AvroDouble => "double"
      case AvroNull => "null"
      case AvroBool => "boolean"
      case AvroLong => "long"
      case AvroBytes => "bytes"
      case _: AvroArray => "array"
      case _: AvroMap => "map"
      case _: AvroRecord => "record"
      case _: AvroEnum => "enum"
    }
}

case object AvroString extends AvroType
case object AvroDouble extends AvroType
case object AvroBool extends AvroType
case object AvroNull extends AvroType
case object AvroLong extends AvroType
case object AvroBytes extends AvroType
case class AvroArray(items: AvroType) extends AvroType
case class AvroMap(values: AvroType) extends AvroType

case class AvroEnum(name: String, symbols: Seq[String]) extends AvroType

//TODO: do something better than Any for default
case class AvroField(name: String, doc: Option[String], `type`: AvroType, default: Any, order: Option[AvroOrder])
case class AvroRecord(name: String, namespace: Option[String], doc: Option[String], fields: Seq[AvroField]) extends AvroType