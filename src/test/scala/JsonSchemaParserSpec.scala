package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


class JsonSchemaParserSpec extends AnyFlatSpec {
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
}
