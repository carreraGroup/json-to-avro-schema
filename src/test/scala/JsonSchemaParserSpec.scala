package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Url
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


class JsonSchemaParserSpec extends AnyFlatSpec {
  it should "parse the $schema URI" in {
    val input = ujson.Obj(
        "$schema" -> "http://example.com/version/schema"
    )

    val Right(schema) = JsonSchemaParser.parse(input)
    schema should be(JsonSchema(Url.parse("http://example.com/version/schema")))
  }

  it should "fail if $schema is missing" in {
    val input = ujson.Obj(
      "notAThing" -> "false"
    )

    val Left(error) = JsonSchemaParser.parse(input)
    error.getMessage should be ("$schema must be specified")
    error.getCause.getMessage should be("key not found: $schema")
  }
}
