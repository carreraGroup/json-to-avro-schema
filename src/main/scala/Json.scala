package io.carrera.jsontoavroschema

package object Json {
  type JSchema = Either[Boolean, JsonSchema]

  sealed trait JsonSchemaType

  case object JsonSchemaNull extends JsonSchemaType
  case object JsonSchemaBool extends JsonSchemaType
  case object JsonSchemaInteger extends JsonSchemaType
  case object JsonSchemaNumber extends JsonSchemaType
  case object JsonSchemaString extends JsonSchemaType
  case object JsonSchemaArray extends JsonSchemaType
  case object JsonSchemaObject extends JsonSchemaType
}
