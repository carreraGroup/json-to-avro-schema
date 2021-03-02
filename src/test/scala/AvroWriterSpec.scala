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

    val records = AvroWriter.toJson(schema)
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

    val records = AvroWriter.toJson(schema)
    records should be(expected)
  }

  it should "write default if available" in {
    val schema =
      AvroRecord("SomeRecord", None, None,
        Seq(AvroField("id", Some("an id"), AvroString, Some(ujson.Null), None))
      )

    val expected = ujson.Obj(
      "type" -> "record",
      "name" -> "SomeRecord",
      "fields" -> ujson.Arr(
        ujson.Obj(
          "name" -> "id",
          "doc" -> "an id",
          "type" -> "string",
          "default" -> ujson.Null
        )
      ),
    )

    val records = AvroWriter.toJson(schema)
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

    val records = AvroWriter.toJson(schema)
    records should be(expected)
  }

  it should "write value type if map" in {
    val schema =
      AvroRecord("Record", None, None,
        Seq(AvroField("boolMap", None, AvroMap(AvroBool), None, None))
      )

    val expected = ujson.Obj(
      "type" -> "record",
      "name" -> "Record",
      "fields" -> ujson.Arr(
        ujson.Obj(
          "name" -> "boolMap",
          "type" -> ujson.Obj(
            "type" -> "map",
            "values" -> "boolean"
          )
        )
      ),
    )

    val records = AvroWriter.toJson(schema)
    records should be(expected)
  }

  it should "write nested records" in {
    val schema = {
      AvroRecord("Record", None, None,
        Seq(AvroField(
          "someObj",
          None,
          AvroRecord("SomeObj", None, None, Seq(AvroField("Inner", None, AvroLong, None, None))),
          None,
          None
        ))
      )
    }
    val expected =
      ujson.Obj(
        "type" -> "record",
        "name" -> "Record",
        "fields" -> ujson.Arr(
          ujson.Obj(
            "name" -> "someObj",
            "type" -> ujson.Obj(
              "type" -> "record",
              "name" -> "SomeObj",
              "fields" -> ujson.Arr(
                ujson.Obj(
                  "name" -> "Inner",
                  "type" -> "long"
                )
              )
            )
          )
        )
      )

    val records = AvroWriter.toJson(schema)
    records should be(expected)
  }

  it should "write enums" in {
    val schema =
      AvroRecord("Record", None, None,
        Seq(AvroField("someProp", None, AvroEnum("somePropEnum", Seq("a","b")), None, None))
      )

    val expected = ujson.Obj(
      "type" -> "record",
      "name" -> "Record",
      "fields" -> ujson.Arr(
        ujson.Obj(
          "name" -> "someProp",
          "type" -> ujson.Obj(
            "type" -> "enum",
            "name" -> "somePropEnum",
            "symbols" -> ujson.Arr("a", "b")
          )
        )
      ),
    )
    val records = AvroWriter.toJson(schema)
    records should be(expected)
  }

}
