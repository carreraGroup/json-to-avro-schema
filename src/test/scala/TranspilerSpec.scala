package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TranspilerSpec extends AnyFlatSpec {

  it should "create a record" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        desc = Some("a description"),
        properties = Map("title" -> JsonSchema.empty.copy(desc = Some("a title"), types = Seq(JsonSchemaString)))
      )

    val Right(avroSchema) = Transpiler.transpile(root, None)

    val expectedRecord =
      AvroRecord(
        "schema",
        None,
        Some("a description"),
        Seq(AvroField("title", Some("a title"), AvroString, None, None))
      )

    avroSchema should be(expectedRecord)
  }

  it should "include namespace in root schema" in {
    val root = JsonSchema.empty.copy(id = schemaUri)
    val Right(avroSchema) = Transpiler.transpile(root, Some("com.example"))
    val expectedRecord = AvroRecord("schema", Some("com.example"), None, Seq())
    avroSchema should be(expectedRecord)
  }

  it should "transpile numbers to doubles" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        properties = Map("maximum" -> JsonSchema.empty.copy(types = Seq(JsonSchemaNumber)))
      )

    val Right(avroSchema) = Transpiler.transpile(root, None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("maximum", None, AvroDouble, None, None)))
    avroSchema should be(expectedRecord)
  }

  it should "transpile integers to longs" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        properties = Map("length" -> JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))
      )

    val Right(avroSchema) = Transpiler.transpile(root, None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("length", None, AvroLong, None, None)))
    avroSchema should be(expectedRecord)
  }

  it should "transpile booleans to booleans" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("uniqueItems" -> JsonSchema.empty.copy(types = Seq(JsonSchemaBool)))
    )
    val Right(avroSchema) = Transpiler.transpile(root, None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("uniqueItems", None, AvroBool, None, None)))

    avroSchema should be(expectedRecord)
  }

  it should "transpile nulls to nulls" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("applesauce" -> JsonSchema.empty.copy(types = Seq(JsonSchemaNull)))
    )
    val Right(avroSchema) = Transpiler.transpile(root, None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("applesauce", None, AvroNull, None, None)))

    avroSchema should be(expectedRecord)
  }

  it should "transpile empty schema to bytes" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("const" -> JsonSchema.empty)
    )
    val Right(avroSchema) = Transpiler.transpile(root, None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("const", None, AvroBytes, None, None)))

    avroSchema should be(expectedRecord)
  }

  it should "transpile arrays with types" in {
    /*
     "stringArray": {
       "type": "array",
       "items": { "type": "string" }
     }
     */

    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("someList" ->
        JsonSchema.empty.copy(
          types = Seq(JsonSchemaArray),
          items = Seq(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))
        )
      )
    )
    val Right(avroSchema) = Transpiler.transpile(root, None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("someList", None, AvroArray(AvroString), None, None)))

    avroSchema should be(expectedRecord)
  }

  it should "transpile arrays of any (empty schema) to arrays of bytes" in {
    /*
      "examples": {
        "type": "array",
        "items": {}
      },
     */

    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("examples" ->
        JsonSchema.empty.copy(
          types = Seq(JsonSchemaArray),
          items = Seq(JsonSchema.empty)
        )
      )
    )
    val Right(avroSchema) = Transpiler.transpile(root, None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("examples", None, AvroArray(AvroBytes), None, None)))

    avroSchema should be(expectedRecord)
  }

  it should "transpile arrays of any to arrays of bytes" in {
    /*
      "someList: {
        "type": "array"
       }
     */
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("someList" -> JsonSchema.empty.copy(types = Seq(JsonSchemaArray)))
    )
    val Right(avroSchema) = Transpiler.transpile(root, None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("someList", None, AvroArray(AvroBytes), None, None)))
    avroSchema should be(expectedRecord)
  }

  it should "transpile objects to map when properties is not present" in {
    /*
      "someObj": {
        "type": "object"
        "additionalProperties": { "type": "string" }
       }
     */
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("someObj" -> JsonSchema.empty.copy(
        types = Seq(JsonSchemaObject),
        additionalProperties = Some(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))
      ))
    )
    val Right(avroSchema) = Transpiler.transpile(root, None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("someObj", None, AvroMap(AvroString), None, None)))
    avroSchema should be(expectedRecord)
  }

  it should "transpile objects to records when an properties is present" in {
    /*
     "someObj": {
       "$id": "SomeObj",
       "type: "object",
       "properties": {
          "Inner": { "type": "integer" }
       }
     }
     */
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("someObj" -> JsonSchema.empty.copy(
        id = Uri.parseOption("SomeObj"),
        types = Seq(JsonSchemaObject),
        properties = Map("Inner" -> JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))
      ))
    )
    val Right(avroSchema) = Transpiler.transpile(root, None)
    val innerRecord = AvroRecord("SomeObj", None, None, Seq(AvroField("Inner", None, AvroLong, None, None)))
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("someObj", None, innerRecord, None, None)))
    avroSchema should be(expectedRecord)
  }

  private def schemaUri =
    Uri.parseOption("http://json-schema.org/draft-06/schema#")
}
