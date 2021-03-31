package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri
import io.lemonlabs.uri.typesafe.dsl._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class SymbolResolverSpec extends AnyFlatSpec {
  private val schemaUri = "http://example.com/schemaName"
  private val schemaUriOption = Uri.parseOption(schemaUri)

  /*
   Given a canonical,
   we need to be able to look up it's ID
   so that we can name the AVRO record appropriately

   Given an ID,
   we need to be able to look up it's canonical,
   so that we can directly look up it's value

   So, we only need to store a record in the symbol table
   IFF it has an ID.
   Otherwise, we can just use it's canonical URI to find and name it.
   We do need to store _both_ whenever we find a record with an ID though.
   */

  it should "return a symbol table given an empty schema" in {
    SymbolResolver.resolve(JsonSchema.empty) should be(Map())
  }

  it should "given definitions contain canonical and id" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        )
      )
    )

    val result = SymbolResolver.resolve(root)
    val Some(canonical) = result.get("#/definitions/A")
    val expectedId = s"$schemaUri#foo"

    canonical should be(expectedId.toUrl)

    val Some(id) = result.get(expectedId)
    id should be("#/definitions/A".toUrl)
  }

  it should "only store schemas with IDs" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> JsonSchema.empty
      )
    )

    val result = SymbolResolver.resolve(root)
    result.get("#/definitions/A") should be(None)
  }

  it should "visit nested schemas" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "B" -> JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo"),
          definitions = Map(
            "X" -> JsonSchema.empty.copy(
              id = Some("http://example.com/other.json#bar")
            )
          )
        )
      )
    )

    val result = SymbolResolver.resolve(root)

    val Some(canonical) = result.get("#/definitions/B/definitions/X")
    val expectedId = "http://example.com/other.json#bar"

    canonical should be(expectedId.toUrl)

    val Some(id) = result.get(expectedId)
    id should be("#/definitions/B/definitions/X".toUrl)
  }

  it should "visit properties" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      properties = Map(
        "A" -> JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/foo"),
          definitions = Map(
            "B" -> JsonSchema.empty.copy(
              id = Uri.parseOption("http://example.com/foo#bar")
            )
          )
        )
      )
    )

    val result = SymbolResolver.resolve(root)

    val Some(canonical) = result.get("#/properties/A/definitions/B")
    val expectedId = "http://example.com/foo#bar"

    canonical should be(expectedId.toUrl)

    val Some(id) = result.get(expectedId)
    id should be("#/properties/A/definitions/B".toUrl)
  }

  ignore should "visit all other schemas" in {
    fail("IdNormalizer has a list of all nodes that may contain schemas")
  }
}
