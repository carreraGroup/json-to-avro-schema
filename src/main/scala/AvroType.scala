package io.carrera.jsontoavroschema

object AvroType extends Enumeration {
  type AvroType = Value
  //TODO: add types for all the primitive and complex types
  val AvroString, AvroDouble, AvroBool, AvroNull, AvroLong = Value

  //TODO: jsonschema parser should encode these as types
  def fromJsonSchema(value: String): AvroType =
    value match {
      case "string" => AvroString
      case "number" => AvroDouble
      case "boolean" => AvroBool
      case "null" => AvroNull
      case "integer" => AvroLong
    }

  def serialize(value: AvroType): String =
    value match {
      case AvroString => "string"
      case AvroDouble => "double"
      case AvroNull => "null"
      case AvroBool => "boolean"
      case AvroLong => "long"
    }
}
