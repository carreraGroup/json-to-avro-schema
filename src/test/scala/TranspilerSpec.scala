package io.carrera.jsontoavroschema

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TranspilerSpec extends AnyFlatSpec {
  it should "take a JsonSchema and return an AvroSchema" in {
    val expected = AvroSchema(None, Seq())
    Transpiler.transpile(emptySchema, None) should be(Right(expected))
  }

  it should "take namespace as an argument" in {
    val namespace = Some("com.example.foo")
    val expected = AvroSchema(namespace, Seq())
    Transpiler.transpile(emptySchema, namespace) should be(Right(expected))
  }

  it should "create a record" in {
    val input = ujson.Obj(
      "definitions" -> ujson.Obj(
        "Element" -> ujson.Obj(
          "description" -> "a description",
          "properties" -> ujson.Obj("id" -> ujson.Obj("type" -> "string")) //fhir ids are not strings, but #/definitions/strings which do a regex validation
        )
      )
    )
    //TODO: make creating JsonSchema easy enough that we don't need to parse it
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(avroSchema) = Transpiler.transpile(root.schema, None)

    val expectedRecord =
      AvroRecord(
        "Element",
        Some("a description"),
        Seq(AvroField("id", None, AvroString(), None, None))
      )

    avroSchema.records.head should be(expectedRecord)
  }

  def emptySchema: JsonSchema = JsonSchema(
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
