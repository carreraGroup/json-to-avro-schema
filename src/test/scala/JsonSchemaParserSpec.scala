package io.carrera.jsontoavroschema

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
    val Some(id) = root.schema.id
    id should be(Uri.parse("http://example.com/schema#"))
  }

  it should "parse an $id that is a URN" in {
    val input = ujson.Obj(
      "$id" -> "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"
    )

    val Right(root) = JsonSchemaParser.parse(input)
    val Some(id) = root.schema.id
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
    val Some(id) = root.schema.ref
    id should be(Uri.parse("#foo"))
  }

  it should "parse title" in {
    val input = ujson.Obj(
      "title" -> "My awesome schema"
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Some(title) = root.schema.title
    title should be("My awesome schema")
  }

  it should "fail if title is not a string" in {
    val input = ujson.Obj(
      "title" -> 32
    )
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("title must be a String")
  }

  it should "parse description" in {
    val input = ujson.Obj(
      "description" -> "this is a pretty useless schema"
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Some(desc) = root.schema.desc
    desc should be("this is a pretty useless schema")
  }

  ignore should "parse default" in {
    /*
     * This can be any JSON value.
     * https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-7.3
     *
     * I have no idea how we'll translate this to an AVRO default.
     * Thankfully, the fhir schema does not use JSON Schema defaults.
     */
  }

  ignore should "parse examples" in {
    /*
     * AVRO doesn't have any equivalent
     * and the values inside the array suffer the same problem as default.
     * They can be any JSON value.
     *
     * https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-7.4
     */
  }

  it should "parse multipleOf" in {
    val input = ujson.Obj(
      "multipleOf" -> 2
    )

    val Right(root) = JsonSchemaParser.parse(input)
    val Some(multipleOf) = root.schema.multipleOf
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
    val Some(max) = root.schema.maximum
    max should be(3)
  }

  it should "parse exclusiveMaximum" in {
    val input = ujson.Obj(
      "exclusiveMaximum" -> 4
    )

    val Right(root) = JsonSchemaParser.parse(input)
    val Some(max) = root.schema.exclusiveMaximum
    max should be(4)
  }

  it should "parse minimum" in {
    val input = ujson.Obj(
      "minimum" -> 5
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Some(min) = root.schema.minimum
    min should be(5)
  }

  it should "parse exclusiveMinimum" in {
    val input = ujson.Obj(
      "exclusiveMinimum" -> 6
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Some(exclMin) = root.schema.exclusiveMinimum
    exclMin should be(6)
  }

  it should "parse maxLength" in {
    val input = ujson.Obj(
      "maxLength" -> 0
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val Some(len) = root.schema.maxLength
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
    root.schema.minLength should be(0)
  }

  it should "parse minLength" in {
    val input = ujson.Obj(
      "minLength" -> 1
    )
    val Right(root) = JsonSchemaParser.parse(input)
    root.schema.minLength should be(1)
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
    val Some(pattern) = root.schema.pattern
    pattern should be("^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")
  }

  it should "parse schema items" in {
    val input = ujson.Obj(
      "items" -> ujson.Obj("$id" -> "#foo")
    )
    val Right(root) = JsonSchemaParser.parse(input)
    val items = root.schema.items
    val Some(innerId) = items.head.id
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
    val head::tail = root.schema.items
    val Some(fooId) = head.id
    fooId should be(Uri.parse("#foo"))
    val Some(barId) = tail.head.id
    barId should be(Uri.parse("#bar"))
  }

  it should "default items to empty list" in {
    val input = ujson.Obj("$id" -> "#foo")
    val Right(root) = JsonSchemaParser.parse(input)
    val items = root.schema.items
    items should be(Seq())
  }

  it should "fail if items is anything but object or array" in {
    val input = ujson.Obj("items" -> ujson.Bool(true))
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("items must be an object or array")
  }

  it should "fail if items contains something that isn't an object" in {
    val input = ujson.Obj(
      "items" -> ujson.Arr(ujson.Bool(true))
    )
    val Left(err) = JsonSchemaParser.parse(input)
    err.getMessage should be("items array contents must be objects")
  }

  it should "parse maxItems" in {
    val input = ujson.Obj("maxItems" -> 32)
    val Right(root) = JsonSchemaParser.parse(input)
    val Some(maxItems) = root.schema.maxItems
    maxItems should be(32)
  }

  it should "parse minItems" in {
    val input = ujson.Obj("minItems" -> 22)
    val Right(root) = JsonSchemaParser.parse(input)
    val minItems = root.schema.minItems
    minItems should be(22)
  }
}
