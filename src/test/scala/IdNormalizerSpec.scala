package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri
import io.lemonlabs.uri.typesafe.dsl._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class IdNormalizerSpec extends AnyFlatSpec {
  private val schemaUri = "http://example.com/schemaName"
  private val schemaUriOption = Uri.parseOption(schemaUri)

  it should "error if root does not have an ID" in {
    val Left(err) = IdNormalizer.normalizeIds(Right(JsonSchema.empty))
    err.getMessage should be("$id must be specified in root schema")
  }

  it should "pass through boolean schemas" in {
    val root = Right(JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> Left(true)
      )
    ))

    val Right(result) = IdNormalizer.normalizeIds(root)
    result should be(root)
  }

  it should "resolve ids in definitions" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      definitions = root.definitions + ("A" ->
        Right(JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        )))
    ))
    result should be(expected)
  }

  it should "resolve ids in nested definitions" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> Right(JsonSchema.empty.copy(
          definitions = Map(
            "B" -> Right(JsonSchema.empty.copy(id = Uri.parseOption("#bar")))
          )
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      definitions = root.definitions + ("A" ->
        Right(JsonSchema.empty.copy(
          definitions = Map(
            "B" -> Right(JsonSchema.empty.copy(id = Some(s"$schemaUri#bar")))
          )
        ))
      )
    ))
    result should be(expected)
  }

  it should "resolve introduce a new namespace at each id" in {
    /*
     * If an ID is not a fragment, it introduces a new namespace
     * as a sibling to the schema root namespace.
     * https://tools.ietf.org/html/draft-wright-json-schema-01#section-9
     */
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> Right(JsonSchema.empty.copy(
          id = Uri.parseOption("foo"),
          definitions = Map(
            "B" -> Right(JsonSchema.empty.copy(id = Uri.parseOption("#bar")))
          )
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      definitions = root.definitions + ("A" ->
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/foo"),
          definitions = Map(
            "B" -> Right(JsonSchema.empty.copy(id = Uri.parseOption("http://example.com/foo#bar")))
          )
        )))
    ))
    result should be(expected)
  }

  it should "pass through URNs" in {
    // they're already unique identifiers and can't be relative
    val root = Right(JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> Right(JsonSchema.empty.copy(
          id = Uri.parseOption("urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"),
        ))
      )
    ))

    val Right(result) = IdNormalizer.normalizeIds(root)

    result should be(root)
  }

  it should "pass through Absolute URLs" in {
    // they're already unique identifiers and can't be relative
    val root = Right(JsonSchema.empty.copy(
      id = schemaUriOption,
      definitions = Map(
        "A" -> Right(JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/foo"),
        ))
      )
    ))

    val Right(result) = IdNormalizer.normalizeIds(root)

    result should be(root)
  }

  it should "visit additionalItems" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      additionalItems = Some(Right(JsonSchema.empty.copy(
        id = Uri.parseOption("foo/bar"),
        additionalItems = Some(Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#baz")
        )))
      )))
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      additionalItems = Some(Right(JsonSchema.empty.copy(
        Uri.parseOption("http://example.com/foo/bar"),
        additionalItems = Some(Right(JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/foo/bar#baz")
        )))
      )))
    ))
    result should be(expected)
  }

  it should "visit contains" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      contains = Some(Right(JsonSchema.empty.copy(
        id = Uri.parseOption("foo/bar"),
        contains = Some(Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#baz")
        )))
      )))
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      contains = Some(Right(JsonSchema.empty.copy(
        Uri.parseOption("http://example.com/foo/bar"),
        contains = Some(Right(JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/foo/bar#baz")
        )))
      )))
    ))
    result should be(expected)
  }

  it should "visit properties" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      properties = Map(
        "A" -> Right(JsonSchema.empty.copy(
          id = Uri.parseOption("foo"),
          definitions = Map(
            "B" -> Right(JsonSchema.empty.copy(id = Uri.parseOption("#bar")))
          )
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      properties = root.definitions + ("A" ->
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/foo"),
          definitions = Map(
            "B" -> Right(JsonSchema.empty.copy(id = Uri.parseOption("http://example.com/foo#bar")))
          )
        )))
    ))
    result should be(expected)
  }

  it should "visit patternProperties" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      patternProperties = Map(
        "A" -> Right(JsonSchema.empty.copy(
          id = Uri.parseOption("foo"),
          properties = Map(
            "B" -> Right(JsonSchema.empty.copy(id = Uri.parseOption("#bar")))
          )
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      patternProperties = root.definitions + ("A" ->
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("http://example.com/foo"),
          properties = Map(
            "B" -> Right(JsonSchema.empty.copy(id = Uri.parseOption("http://example.com/foo#bar")))
          )
        )))
    ))
    result should be(expected)
  }

  it should "visit additionalProperties" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      additionalProperties = Some(
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      additionalProperties = Some(
        Right(JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        ))
      )
    ))
    result should be(expected)
  }

  it should "visit dependencies" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      dependencies = Map("a" ->
        Right(Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        )))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      dependencies = Map("a" ->
        Right(Right(JsonSchema.empty.copy(
          id = Uri.parseOption(s"$schemaUri#foo")
        )))
      )
    ))
    result should be(expected)
  }

  it should "visit propertyNames" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      propertyNames = Some(
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      propertyNames = Some(
        Right(JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        ))
      )
    ))
    result should be(expected)
  }

  it should "visit allOf" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      allOf = Seq(
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      allOf = Seq(
        Right(JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        ))
      )
    ))
    result should be(expected)
  }

  it should "visit anyOf" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      anyOf = Seq(
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      anyOf = Seq(
        Right(JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        ))
      )
    ))
    result should be(expected)
  }

  it should "visit oneOf" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      oneOf = Seq(
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      oneOf = Seq(
        Right(JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        ))
      )
    ))
    result should be(expected)
  }

  it should "visit not" in {
    val root = JsonSchema.empty.copy(
      id = schemaUriOption,
      not = Some(
        Right(JsonSchema.empty.copy(
          id = Uri.parseOption("#foo")
        ))
      )
    )

    val Right(result) = IdNormalizer.normalizeIds(Right(root))

    val expected = Right(root.copy(
      not = Some(
        Right(JsonSchema.empty.copy(
          id = Some(s"$schemaUri#foo")
        ))
      )
    ))
    result should be(expected)
  }
}
