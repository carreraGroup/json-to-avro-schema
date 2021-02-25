package io.carrera.jsontoavroschema

sealed trait AvroType {
  def serialize(): String =
    this match {
      case AvroString => "string"
      case AvroDouble => "double"
      case AvroNull => "null"
      case AvroBool => "boolean"
      case AvroLong => "long"
      case AvroBytes => "bytes"
      case AvroArray(_) => "array"
    }
}

object AvroType {
  //TODO: jsonschema parser should encode these as types
  def fromJsonSchema(types: Seq[String], items: Seq[JsonSchema]): Either[String, AvroType] = {
    types match {
      case Nil => Right(AvroBytes)
      case value::Nil =>
        value match {
          case "string" => Right(AvroString)
          case "number" => Right(AvroDouble)
          case "boolean" => Right(AvroBool)
          case "null" => Right(AvroNull)
          case "integer" => Right(AvroLong)
          case "array" =>
            items match {
              case Nil =>
                for {
                  itemType <- fromJsonSchema(Seq(), Seq())
                } yield AvroArray(itemType)
              case x::Nil =>
                for {
                  itemType <- fromJsonSchema(x.types, x.items)
                } yield AvroArray(itemType)
              case x::xs => Left("Unimplemented: array items must have a single type")
            }
          case _ => Left(s"Unexpected JSON type: $value")
        }
      case x::xs => Left("Unimplemented: unions aren't supported yet")
    }
  }
}

case object AvroString extends AvroType
case object AvroDouble extends AvroType
case object AvroBool extends AvroType
case object AvroNull extends AvroType
case object AvroLong extends AvroType
case object AvroBytes extends AvroType
case class AvroArray(items: AvroType) extends AvroType
