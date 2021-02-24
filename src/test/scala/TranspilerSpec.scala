package io.carrera.jsontoavroschema

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TranspilerSpec extends AnyFlatSpec {

  it should "create a record" in {
    val input = ujson.Obj(
      "$id" -> "http://json-schema.org/draft-06/schema#",
      "description" -> "a description",
      "properties" -> ujson.Obj(
        "title" -> ujson.Obj(
          "description" -> "a title",
          "type" -> "string",
        )
      )
    )
    //TODO: make creating JsonSchema easy enough that we don't need to parse it
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(avroSchema) = Transpiler.transpile(root.schema, None)

    val expectedRecord =
      AvroRecord(
        "schema",
        None,
        Some("a description"),
        Seq(AvroField("title", Some("a title"), AvroString(), None, None))
      )

    avroSchema should be(expectedRecord)
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
