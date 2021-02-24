package io.carrera.jsontoavroschema

object AvroOrder extends Enumeration {
  type AvroOrder = Value
  val Ascending, Descending, Ignore = Value
}