package io.carrera.jsontoavroschema

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class AvroWriterSpec extends AnyFlatSpec {
  it should "create json object" in {
    val schema =
        AvroRecord(
          "Element",
          None,
          Some("a description"),
          Seq(AvroField("id", Some("an id"), AvroString, None, None))
        )

    val expected = ujson.Obj(
      "type" -> "record",
      "name" -> "Element",
      "doc" -> "a description",
      "fields" -> ujson.Arr(
        ujson.Obj(
          "name" -> "id",
          "doc" -> "an id",
          "type" -> "string",
        )
      ),
    )

    val Right(records) = AvroWriter.toJson(schema)
    records should be(expected)
  }

  it should "write namespace if available" in {
    val schema =
      AvroRecord(
        "Element",
        Some("com.example"),
        Some("a description"),
        Seq(AvroField("id", Some("an id"), AvroString, None, None))
      )

    val expected = ujson.Obj(
      "type" -> "record",
      "name" -> "Element",
      "namespace" -> "com.example",
      "doc" -> "a description",
      "fields" -> ujson.Arr(
        ujson.Obj(
          "name" -> "id",
          "doc" -> "an id",
          "type" -> "string",
        )
      ),
    )

    val Right(records) = AvroWriter.toJson(schema)
    records should be(expected)
  }

  it should "write item type if array" in {
    val schema =
      AvroRecord("Element", None, None,
        Seq(AvroField("stringArray", None, AvroArray(AvroString), None, None))
      )

    val expected = ujson.Obj(
      "type" -> "record",
      "name" -> "Element",
      "fields" -> ujson.Arr(
        ujson.Obj(
          "name" -> "stringArray",
          "type" -> ujson.Obj(
            "type" -> "array",
            "items" -> "string"
          )
        )
      ),
    )

    val Right(records) = AvroWriter.toJson(schema)
    records should be(expected)
  }
}
