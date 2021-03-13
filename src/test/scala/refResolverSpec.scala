package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri
import io.lemonlabs.uri.typesafe.dsl._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class refResolverSpec extends AnyFlatSpec {
  val schemaUri = "http://example.com/schemaName"
  val schemaUriOption = Uri.parseOption(schemaUri)

  it should "error if root does not have an ID" in {
    val Left(err) = RefResolver.normalizeIds(JsonSchema.empty)
    err.getMessage should be("$id must be specified in root schema")
  }

  it should "resolve ids in definitions" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        )
      )
    )

    val Right(result) = RefResolver.normalizeIds(root)

    val expected = root.copy(
      definitions = root.definitions + ("A" ->
        JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        ))
    )
    result should be(expected)
  }

  it should "resolve ids in nested definitions" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> JsonSchema.empty.copy(
          definitions = Map(
            "B" -> JsonSchema.empty.copy(id = Uri.parseOption("#bar"))
          )
        )
      )
    )

    val Right(result) = RefResolver.normalizeIds(root)

    val expected = root.copy(
      definitions = root.definitions + ("A" ->
        JsonSchema.empty.copy(
          definitions = Map(
            "B" -> JsonSchema.empty.copy(id = Some(s"$schemaUri#bar"))
          )
        )
      )
    )
    result should be(expected)
  }

  it should "resolve introduce a new namespace at each id" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> JsonSchema.empty.copy(
          id = Uri.parseOption("foo"),
          definitions = Map(
            "B" -> JsonSchema.empty.copy(id = Uri.parseOption("#bar"))
          )
        )
      )
    )

    val Right(result) = RefResolver.normalizeIds(root)

    val expected = root.copy(
      definitions = root.definitions + ("A" ->
        JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/schemaName/foo"),
          definitions = Map(
            "B" -> JsonSchema.empty.copy(id = Uri.parseOption("http://example.com/schemaName/foo#bar"))
          )
        ))
    )
    result should be(expected)
  }

  it should "visit additionalItems" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      additionalItems = Some(JsonSchema.empty.copy(
        id = Uri.parseOption("foo/bar"),
        additionalItems = Some(JsonSchema.empty.copy(
          id = Uri.parseOption("baz")
        ))
      ))
    )

    val Right(result) = RefResolver.normalizeIds(root)

    val expected = root.copy(
      additionalItems = Some(JsonSchema.empty.copy(
        Uri.parseOption("http://example.com/schemaName/foo/bar"),
        additionalItems = Some(JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/schemaName/foo/bar/baz")
        ))
      ))
    )
    result should be(expected)
  }

  it should "visit contains" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      contains = Some(JsonSchema.empty.copy(
        id = Uri.parseOption("foo/bar"),
        contains = Some(JsonSchema.empty.copy(
          id = Uri.parseOption("baz")
        ))
      ))
    )

    val Right(result) = RefResolver.normalizeIds(root)

    val expected = root.copy(
      contains = Some(JsonSchema.empty.copy(
        Uri.parseOption("http://example.com/schemaName/foo/bar"),
        contains = Some(JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/schemaName/foo/bar/baz")
        ))
      ))
    )
    result should be(expected)
  }

  it should "visit properties" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      properties = Map(
        "A" -> JsonSchema.empty.copy(
          id = Uri.parseOption("foo"),
          definitions = Map(
            "B" -> JsonSchema.empty.copy(id = Uri.parseOption("#bar"))
          )
        )
      )
    )

    val Right(result) = RefResolver.normalizeIds(root)

    val expected = root.copy(
      properties = root.definitions + ("A" ->
        JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/schemaName/foo"),
          definitions = Map(
            "B" -> JsonSchema.empty.copy(id = Uri.parseOption("http://example.com/schemaName/foo#bar"))
          )
        ))
    )
    result should be(expected)
  }

  ignore should "visit patternProperties"
  ignore should "visit additionalProperties"
  ignore should "visit dependencies"
  ignore should "visit propertyNames"
  ignore should "visit allOf"
  ignore should "visit anyOf"
  ignore should "visit oneOf"
  ignore should "visit not"

  //TODO: visit all nodes that could have a schema
}
