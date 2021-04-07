package io.carrera.jsontoavroschema

import Json._
import io.lemonlabs.uri.Uri
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class JsonSchemaParserSpec extends AnyFlatSpec {

  it should "fail if root is not an Object" in {
    val input = ujson.Arr(1,2,3)
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("schema must be an object")
  }

  it should "parse the $schema URI" in {
    val input = ujson.Obj(
        "$schema" -> "http://example.com/version/schema"
    )

    val Right(schema) = JsonSchemaParser.parse(input)
    val Some(schemaUri) = schema.schemaUri
    schemaUri should be(Uri.parse("http://example.com/version/schema"))
  }

  it should "$schema is optional" in {
    val input = ujson.Obj(
      "notAThing" -> "false"
    )

    val Right(schema) = JsonSchemaParser.parse(input)
    schema.schemaUri should be (None)
  }

  it should "parse the $id" in {
    val input = ujson.Obj(
      "$id" -> "http://example.com/schema#"
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(id) = schema.id
    id should be(Uri.parse("http://example.com/schema#"))
  }

  it should "parse an $id that is a URN" in {
    val input = ujson.Obj(
      "$id" -> "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"
    )

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(id) = schema.id
    id should be(Uri.parse("urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"))
  }

  it should "parse ref" in {
    val input = ujson.Obj(
      // can be any uri reference
      // https://tools.ietf.org/html/draft-wright-json-schema-01#section-8
      // https://tools.ietf.org/html/rfc3986#section-4.1
      "$ref" -> "#foo"
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(id) = schema.ref
    id should be(Uri.parse("#foo"))
  }

  it should "parse title" in {
    val input = ujson.Obj(
      "title" -> "My awesome schema"
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(title) = schema.title
    title should be("My awesome schema")
  }

  it should "fail if title is not a string" in {
    val input = ujson.Obj(
      "title" -> 32
    )
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("title must be a string")
  }

  it should "parse description" in {
    val input = ujson.Obj(
      "description" -> "this is a pretty useless schema"
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(desc) = schema.desc
    desc should be("this is a pretty useless schema")
  }

  it should "parse default" in {
    val input = ujson.Obj("default" -> ujson.Bool(true))
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    // can be any ujson.Value
    val Some(default) = schema.default
    default should be(ujson.Bool(true))
  }

  ignore should "parse examples" in {
    /*
     * AVRO doesn't seem to have any equivalent
     *
     * https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-7.4
     */
  }

  it should "parse multipleOf" in {
    val input = ujson.Obj(
      "multipleOf" -> 2
    )

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(multipleOf) = schema.multipleOf
    multipleOf should be(2)
  }

  it should "fail if multipleOf is <= 0" in {
    val input = ujson.Obj(
      "multipleOf" -> 0
    )

    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("multipleOf must be > 0")
  }

  it should "parse maximum" in {
    val input = ujson.Obj(
      "maximum" -> 3
    )

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(max) = schema.maximum
    max should be(3)
  }

  it should "parse exclusiveMaximum" in {
    val input = ujson.Obj(
      "exclusiveMaximum" -> 4
    )

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(max) = schema.exclusiveMaximum
    max should be(4)
  }

  it should "parse minimum" in {
    val input = ujson.Obj(
      "minimum" -> 5
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(min) = schema.minimum
    min should be(5)
  }

  it should "parse exclusiveMinimum" in {
    val input = ujson.Obj(
      "exclusiveMinimum" -> 6
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(exclMin) = schema.exclusiveMinimum
    exclMin should be(6)
  }

  it should "parse maxLength" in {
    val input = ujson.Obj(
      "maxLength" -> 0
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(len) = schema.maxLength
    len should be(0)
  }

  it should "fail if maxLength is < 0" in {
    val input = ujson.Obj(
      "maxLength" -> -1
    )
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("maxLength must be >= 0")
  }

  it should "have a default minLength of zero" in {
    val input = ujson.Obj(
      "maxLength" -> 1
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.minLength should be(0)
  }

  it should "parse minLength" in {
    val input = ujson.Obj(
      "minLength" -> 1
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.minLength should be(1)
  }

  it should "fail if minLength < 0" in {
    val input = ujson.Obj(
      "minLength" -> -1
    )
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("minLength must be >= 0")
  }

  it should "parse pattern" in {
    val input = ujson.Obj(
      "pattern" -> "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(pattern) = schema.pattern
    pattern should be("^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")
  }

  it should "parse schema items" in {
    val input = ujson.Obj(
      "items" -> ujson.Obj("$id" -> "#foo")
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val items = schema.items
    val Right(firstItem) = items.head
    val Some(innerId) = firstItem.id
    innerId should be(Uri.parse("#foo"))
  }

  it should "parse array of items" in {
    val input = ujson.Obj(
      "items" -> ujson.Arr(
        ujson.Obj("$id" -> "#foo"),
        ujson.Obj("$id" -> "#bar")
      )
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Right(head)::tail = schema.items
    val Some(fooId) = head.id
    fooId should be(Uri.parse("#foo"))
    val Right(secondItem) = tail.head
    val Some(barId) = secondItem.id
    barId should be(Uri.parse("#bar"))
  }

  it should "parse additionalItems" in {
    val input = ujson.Obj(
      "additionalItems" -> ujson.Obj("$id" -> "#foo")
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(Right(additionalItems)) = schema.additionalItems
    additionalItems.id should be(Some(Uri.parse("#foo")))
  }

  it should "default items to empty list" in {
    val input = ujson.Obj("$id" -> "#foo")
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val items = schema.items
    items should be(Seq())
  }

  it should "fail if items is anything but object or array" in {
    val input = ujson.Obj("items" -> ujson.Bool(true))
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("items must be an object or array")
  }

  it should "fail if items contains something that isn't an object or bool" in {
    val input = ujson.Obj(
      "items" -> ujson.Arr(ujson.Str("boom"))
    )
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("items array contents must be objects")
  }

  it should "parse maxItems" in {
    val input = ujson.Obj("maxItems" -> 32)
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(maxItems) = schema.maxItems
    maxItems should be(32)
  }

  it should "parse minItems" in {
    val input = ujson.Obj("minItems" -> 22)
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val minItems = schema.minItems
    minItems should be(22)
  }

  it should "parse uniqueItems" in {
    val input = ujson.Obj("uniqueItems" -> true)
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.uniqueItems should be(true)
  }

  it should "uniqueItems defaults to false" in {
    val input = ujson.Obj("$id" -> "#foo")
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.uniqueItems should be(false)
  }

  it should "fail if uniqueItems value is not a bool" in {
    val input = ujson.Obj("uniqueItems" -> ujson.Str("boom"))
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("uniqueItems must be a boolean")
  }

  it should "parse contains" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj("contains" -> fooSchema)
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(contains) = schema.contains
    contains should be(foo)
  }

  it should "parse maxProperties" in {
    val input = ujson.Obj("maxProperties" -> 0)
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(maxProps) = schema.maxProperties
    maxProps should be(0)
  }

  it should "parse minProperties" in {
    val input = ujson.Obj("minProperties" -> 221)
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.minProperties should be(221)
  }

  it should "parse required" in {
    val input = ujson.Obj(
      "required" -> ujson.Arr("someProperty", "someOtherProp")
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.required should be(Seq("someProperty", "someOtherProp"))
  }

  it should "default required to empty array" in {
    val input = ujson.Obj("$id" -> "#foo")
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.required should be(Seq())
  }

  it should "fail if required not an array" in {
    val input = ujson.Obj("required" -> ujson.Bool(false))
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("required must be an array")
  }

  it should "fail if required array items are not strings" in {
    val input = ujson.Obj(
      "required" -> ujson.Arr(ujson.Bool(true))
    )
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("required value must be a string")
  }

  it should "parse properties" in {
    val fooProp = ujson.Obj("$id" -> "#foo")
    val barProp = ujson.Obj("$id" -> "#bar")
    val input = ujson.Obj(
      "properties" -> ujson.Obj(
        "foo" -> fooProp,
        "bar" -> barProp
      )
    )

    val Right(expectedFoo) = JsonSchemaParser.parseSubSchema(fooProp)
    val Right(expectedBar) = JsonSchemaParser.parseSubSchema(barProp)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val props = schema.properties
    props should be(Map("foo" -> expectedFoo, "bar" -> expectedBar))
  }

  it should "default properties to empty map" in {
    val input = ujson.Obj("$id" -> "#foo")
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.properties should be(Map())
  }

  it should "parse patternProperties" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj(
      "patternProperties" -> ujson.Obj(
        "^S_" -> fooSchema
      )
    )
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.patternProperties should be(Map("^S_" -> foo))
  }

  it should "parse additionalProperties" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj("additionalProperties" -> fooSchema)
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(additionalProperties) = schema.additionalProperties
    additionalProperties should be(foo)
  }

  it should "parse dependencies object" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj(
      "dependencies" -> ujson.Obj("foo" -> fooSchema)
    )
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val deps = schema.dependencies
    val Right(actual) = deps("foo")
    actual should be(foo)
  }

  it should "parse dependencies array" in {
    val input = ujson.Obj(
      "dependencies" -> ujson.Obj(
        "foo" -> ujson.Arr("bar", "baz", "qux")
      )
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val deps = schema.dependencies
    val Left(actual) = deps("foo")
    actual should be(Seq("bar","baz","qux"))
  }

  it should "parse propertyNames" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj("propertyNames" -> fooSchema)
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(propertyNames) = schema.propertyNames
    propertyNames should be(foo)
  }

  it should "parse definitions" in {
    val fooProp = ujson.Obj("$id" -> "#foo")
    val barProp = ujson.Obj("$id" -> "#bar")
    val input = ujson.Obj(
      "definitions" -> ujson.Obj(
        "foo" -> fooProp,
        "bar" -> barProp
      )
    )

    val Right(expectedFoo) = JsonSchemaParser.parseSubSchema(fooProp)
    val Right(expectedBar) = JsonSchemaParser.parseSubSchema(barProp)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val props = schema.definitions
    props should be(Map("foo" -> expectedFoo, "bar" -> expectedBar))
  }

  it should "parse const" in {
    val input = ujson.Obj("const" -> ujson.Str("hi"))
    val Right(root) = JsonSchemaParser.parse(input)
    // can be any ujson.Value
    // https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.24
    val Right(schema) = root.schema
    val Some(const) = schema.const
    const should be(ujson.Str("hi"))
  }

  it should "parse string type" in {
    val input = ujson.Obj("type" -> ujson.Str("string"))
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.types should be(Seq(JsonSchemaString))
  }

  it should "parse array type" in {
    val input = ujson.Obj(
      "type" -> ujson.Arr("string", "boolean")
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.types should be(Seq(JsonSchemaString, JsonSchemaBool))
  }

  it should "parse enum" in {
    // enum is a sum type (discriminated union)
    val input = ujson.Obj(
      "enum" -> ujson.Arr(ujson.Str("somevalue"), ujson.Bool(true))
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.enum should be(Seq(ujson.Str("somevalue"), ujson.Bool(true)))
  }

  it should "parse format" in {
    val input = ujson.Obj("format" -> ujson.Str("email"))
    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(format) = schema.format
    format should be("email")
  }

  it should "parse allOf" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj(
      "allOf" -> ujson.Arr(fooSchema)
    )
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.allOf should be(Seq(foo))
  }

  it should "parse anyOf" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj(
      "anyOf" -> ujson.Arr(fooSchema)
    )
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.anyOf should be(Seq(foo))
  }

  it should "parse oneOf" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj(
      "oneOf" -> ujson.Arr(fooSchema)
    )
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    schema.oneOf should be(Seq(foo))
  }

  it should "parse not" in {
    val fooSchema = ujson.Obj("$id" -> "#foo")
    val input = ujson.Obj("not" -> fooSchema)
    val Right(foo) = JsonSchemaParser.parseSubSchema(fooSchema)

    val Right(root) = JsonSchemaParser.parse(input)
    val Right(schema) = root.schema
    val Some(not) = schema.not
    not should be(foo)
  }

  it should "parse boolean schemas" in {
    val input = ujson.Obj("not" -> false)
    val Right(parseResult) = JsonSchemaParser.parse(input)
    val Right(root) = parseResult.schema
    val Some(Left(value)) = root.not
    value should be(false)
  }
}
