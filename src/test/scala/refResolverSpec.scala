package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class refResolverSpec extends AnyFlatSpec {
  val schemaUri = Uri.parseOption("http://example.com/schemaName")

  it should "error if root does not have an ID" in {
    val Left(err) = RefResolver.normalizeIds(JsonSchema.empty)
    err.getMessage should be("$id must be specified in root schema")
  }

  it should "resolve ids in definitions" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
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
          id = buildAbsolute(schemaUri, "#foo"))
        )
    )
    result should be(expected)
  }

  it should "resolve ids in nested definitions" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
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
            "B" -> JsonSchema.empty.copy(id = buildAbsolute(schemaUri, "#bar"))
          )
        )
      )
    )
    result should be(expected)
  }

  it should "resolve introduce a new namespace at each id" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
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


  private def buildAbsolute(maybeBase: Option[Uri], relative: String):Option[Uri] =
    maybeBase.map(uri => buildAbsolute(uri, relative))

  private def buildAbsolute(base: Uri, relative: String):Uri =
      Uri.parse(base + relative)
  //TODO: visit all nodes that could have a schema

}
