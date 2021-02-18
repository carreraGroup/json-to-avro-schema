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
}
