package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Url
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TranspilerSpec extends AnyFlatSpec {
  it should "take a JsonSchema and return an AvroSchema" in {
    val expected = AvroSchema(None, Seq())
    Transpiler.transpile(emptySchema) should be(Right(expected))
  }

  it should "take namespace as an argument" in {
    val expected = AvroSchema(Some("com.example.foo"), Seq())
    Transpiler.transpile(emptySchema, "com.example.foo") should be(Right(expected))
  }

  val emptySchema: JsonSchema = JsonSchema(
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
