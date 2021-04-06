package io.carrera.jsontoavroschema

import Thing.JSchema

import io.lemonlabs.uri.Uri

package object Thing {
  type JSchema = Either[Boolean, JsonSchema]
}

sealed trait JsonSchemaType

case object JsonSchemaNull extends JsonSchemaType
case object JsonSchemaBool extends JsonSchemaType
case object JsonSchemaInteger extends JsonSchemaType
case object JsonSchemaNumber extends JsonSchemaType
case object JsonSchemaString extends JsonSchemaType
case object JsonSchemaArray extends JsonSchemaType
case object JsonSchemaObject extends JsonSchemaType

case class RootJsonSchema(schemaUri: Option[Uri], schema: JSchema)

case class JsonSchema(
                       id: Option[Uri],
                       ref: Option[Uri],
                       title: Option[String],
                       desc: Option[String],
                       definitions: Map[String, JSchema],
                       default: Option[ujson.Value],
                       multipleOf: Option[Double],
                       maximum: Option[Double],
                       exclusiveMaximum: Option[Double],
                       minimum: Option[Double],
                       exclusiveMinimum: Option[Double],
                       maxLength: Option[Int],
                       minLength: Int,
                       pattern: Option[String],
                       items: Seq[JSchema],
                       additionalItems: Option[JSchema],
                       maxItems: Option[Int],
                       minItems: Int,
                       uniqueItems: Boolean,
                       contains: Option[JSchema],
                       maxProperties: Option[Int],
                       minProperties: Int,
                       required: Seq[String],
                       properties: Map[String, JSchema],
                       patternProperties: Map[String, JSchema],
                       additionalProperties: Option[JSchema],
                       dependencies: Map[String, Either[Seq[String], JSchema]],
                       propertyNames: Option[JSchema],
                       const: Option[ujson.Value],
                       types: Seq[JsonSchemaType],
                       enum: Seq[ujson.Value],
                       format: Option[String],
                       allOf: Seq[JSchema],
                       anyOf: Seq[JSchema],
                       oneOf: Seq[JSchema],
                       not: Option[JSchema],
                     )

object JsonSchema {
  def empty: JsonSchema =
    JsonSchema(
      id = None,
      ref = None,
      title = None,
      desc = None,
      definitions = Map(),
      default = None,
      multipleOf= None,
      maximum= None,
      exclusiveMaximum= None,
      minimum= None,
      exclusiveMinimum= None,
      maxLength= None,
      minLength= 0,
      pattern= None,
      items= Seq(),
      additionalItems= None,
      maxItems= None,
      minItems= 0,
      uniqueItems = false,
      contains= None,
      maxProperties= None,
      minProperties= 0,
      required= Seq(),
      properties= Map(),
      patternProperties= Map(),
      additionalProperties= None,
      dependencies= Map(),
      propertyNames= None,
      const = None,
      types= Seq(),
      enum = Seq(),
      format= None,
      allOf= Seq(),
      anyOf= Seq(),
      oneOf= Seq(),
      not= None,
    )
}
