package io.carrera.jsontoavroschema

import Json._
import io.lemonlabs.uri.Uri
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TranspilerSpec extends AnyFlatSpec {

  it should "create a record" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        desc = Some("a description"),
        properties = Map("title" -> Right(JsonSchema.empty.copy(desc = Some("a title"), types = Seq(JsonSchemaString)))),
        required = Seq("title")
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

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
    val Right(avroSchema) = Transpiler.transpile(Right(root), Some("com.example"))
    val expectedRecord = AvroRecord("schema", Some("com.example"), None, Seq())
    avroSchema should be(expectedRecord)
  }

  it should "transpile numbers to doubles" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        properties = Map("maximum" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaNumber)))),
        required = Seq("maximum")
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("maximum", None, AvroDouble, None, None)))
    avroSchema should be(expectedRecord)
  }

  it should "transpile integers to longs" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        properties = Map("length" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))),
        required = Seq("length")
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("length", None, AvroLong, None, None)))
    avroSchema should be(expectedRecord)
  }

  it should "transpile optional values" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        properties = Map("length" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))),
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("length", None, AvroUnion(Seq(AvroNull, AvroLong)), Some(ujson.Null), None)))
    avroSchema should be(expectedRecord)
  }

  it should "transpile booleans to booleans" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("uniqueItems" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaBool)))),
      required = Seq("uniqueItems")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("uniqueItems", None, AvroBool, None, None)))

    avroSchema should be(expectedRecord)
  }

  it should "transpile nulls to nulls" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("applesauce" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaNull)))),
      required = Seq("applesauce")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("applesauce", None, AvroNull, None, None)))

    avroSchema should be(expectedRecord)
  }

  it should "transpile empty schema to bytes" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("const" -> Right(JsonSchema.empty)),
      required = Seq("const")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
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
        Right(JsonSchema.empty.copy(
          types = Seq(JsonSchemaArray),
          items = Seq(Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString))))
        ))
      ),
      required = Seq("someList")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
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
        Right(JsonSchema.empty.copy(
          types = Seq(JsonSchemaArray),
          items = Seq(Right(JsonSchema.empty))
        ))
      ),
      required = Seq("examples")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
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
      properties = Map("someList" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaArray)))),
      required = Seq("someList")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
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
      properties = Map("someObj" -> Right(JsonSchema.empty.copy(
        types = Seq(JsonSchemaObject),
        additionalProperties = Some(Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString))))
      ))),
      required = Seq("someObj")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
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
      properties = Map("someObj" -> Right(JsonSchema.empty.copy(
        id = Uri.parseOption("SomeObj"),
        types = Seq(JsonSchemaObject),
        properties = Map("Inner" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))),
        required = Seq("Inner")
      ))),
      required = Seq("someObj")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
    val innerRecord = AvroRecord("SomeObj", None, None, Seq(AvroField("Inner", None, AvroLong, None, None)))
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("someObj", None, innerRecord, None, None)))
    avroSchema should be(expectedRecord)
  }

  it should "transpile string enums" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("someProp" -> Right(JsonSchema.empty.copy(
          `enum` = Seq(ujson.Str("a"), ujson.Str("b"))
        )
      )),
      required = Seq("someProp")
    )
    val Right(avro) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None,
        Seq(AvroField("someProp", None, AvroEnum("schema_someProp", Seq("a","b")), None, None))
      )

    avro should be(expectedRecord)
  }

  it should "sanitize enum symbols" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("someProp" -> Right(JsonSchema.empty.copy(
        `enum` = Seq(
          ujson.Str("a-thing-a-ma-bob"),
          ujson.Str("text/cql"),
          ujson.Str("Some.thing"),
          ujson.Str("<"),
          ujson.Str("<="),
          ujson.Str(">="),
          ujson.Str(">"),
          ujson.Str("="),
        )
      ))),
      required = Seq("someProp")
    )
    val Right(avro) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None,
        Seq(AvroField("someProp", None, AvroEnum("schema_someProp",
          Seq("a_thing_a_ma_bob", "text_cql", "Some_thing", "LT", "LTEq", "GTEq", "GT", "Eq")),
          None, None
        ))
      )

    avro should be(expectedRecord)
  }

  it should "fail if json enum values are not strings" in {
    /*
     * We should really find a different avro type to transpile to instead.
     * See issue: https://github.com/carreragroup/json-to-avro-schema/issues/22
     */
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("someProp" -> Right(JsonSchema.empty.copy(
        `enum` = Seq(ujson.Str("a"), ujson.Bool(false))
        )
      )),
      required = Seq("someProp")
    )
    val Left(err) = Transpiler.transpile(Right(root), None)
    err.message should be("Unimplemented: non-string enums aren't supported yet at someProp. Value: false")
  }

  it should "transpile type unions" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map("unionType" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaBool, JsonSchemaString)))),
      required = Seq("unionType")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)
    val expectedRecord =
      AvroRecord("schema", None, None, Seq(AvroField("unionType", None, AvroUnion(Seq(AvroBool, AvroString)), None, None)))

    avroSchema should be(expectedRecord)
  }

  it should "transpile a record without an id" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map(
        "A" -> Right(JsonSchema.empty.copy(
          properties = Map(
            "name" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString))),
            "index" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))
          ),
          required = Seq("name", "index")
        ))
      ),
      required = Seq("A")
    )
    val Right(avro) = Transpiler.transpile(Right(root), None)

    val expected =
      AvroRecord("schema", None, None,
        Seq(
          AvroField("A", None,
            AvroRecord("A", None, None,
              Seq(
                AvroField("name", None, AvroString, None, None),
                AvroField("index", None, AvroLong, None, None)
              )
            ), None, None
          ),
        )
      )

    avro should be(expected)
  }

  it should "resolve reference to a record" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map(
        "A" -> Right(JsonSchema.empty.copy(
          properties = Map(
            "name" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString))),
            "index" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))
          ),
          required = Seq("name", "index")
        )),
        "B" -> Right(JsonSchema.empty.copy(
          ref = Uri.parseOption("#/properties/A")
        ))
      ),
      required = Seq("A","B")
    )
    val Right(avro) = Transpiler.transpile(Right(root), None)

    val expected =
      AvroRecord("schema", None, None,
        Seq(
          AvroField("A", None,
            AvroRecord("A", None, None,
              Seq(
                AvroField("name", None, AvroString, None, None),
                AvroField("index", None, AvroLong, None, None)
              )
            ), None, None
          ),
          AvroField("B", None, AvroRef("A"), None, None)
        )
      )

    avro should be(expected)
  }

  it should "resolve reference id of a record if available" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      properties = Map(
        "A" -> Right(JsonSchema.empty.copy(
          id = Uri.parseOption("AwesomeSchema"),
          properties = Map(
            "name" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString))),
            "index" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))
          ),
          required = Seq("name", "index")
        )),
        "B" -> Right(JsonSchema.empty.copy(
          ref = Uri.parseOption("#/properties/A")
        ))
      ),
      required = Seq("A","B")
    )

    val Right(avro) = Transpiler.transpile(Right(root), None)

    avro.fields.filter(f => f.name == "B").head.`type` should be(AvroRef("AwesomeSchema"))
  }

  it should "inline first definition reference" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map("A" -> Right(JsonSchema.empty.copy(
        types = Seq(JsonSchemaInteger)
      ))),
      properties = Map("B" -> Right(JsonSchema.empty.copy(
        ref = Uri.parseOption("#/definitions/A")
      ))),
      required = Seq("B")
    )

    val Right(avro) = Transpiler.transpile(Right(root), None)

    val expected = AvroRecord("A", None, None, Seq(
      AvroField("value", None, AvroLong, None, None)
    ))
    avro.fields.filter(f => f.name == "B").head.`type` should be(expected)
  }

  it should "reference subsequent definition references by name" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map("A" -> Right(JsonSchema.empty.copy(
        types = Seq(JsonSchemaInteger)
      ))),
      properties = Map("B" -> Right(JsonSchema.empty.copy(
          ref = Uri.parseOption("#/definitions/A")
        )),
        "C" -> Right(JsonSchema.empty.copy(
          ref = Uri.parseOption("#/definitions/A")
        ))
      ),
      required = Seq("B","C")
    )

    val Right(avro) = Transpiler.transpile(Right(root), None)

    val expected = AvroRef("A")
    avro.fields.filter(f => f.name == "C").head.`type` should be(expected)
  }

  it should "handle definitions with properties" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map(
        "A" -> Right(JsonSchema.empty.copy(
          properties = Map(
            "name" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString))),
            "index" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaInteger)))
          ),
          required = Seq("name", "index")
        ))
      ),
      properties = Map(
        "B" -> Right(JsonSchema.empty.copy(
          ref = Uri.parseOption("#/definitions/A")
        ))
      ),
      required = Seq("B")
    )

    val Right(avro) = Transpiler.transpile(Right(root), None)

    val expected = AvroRecord(
      "A", None, None,
      Seq(
        AvroField("name", None, AvroString, None, None),
        AvroField("index", None, AvroLong, None, None)
      )
    )
    avro.fields.filter(f => f.name == "B").head.`type` should be(expected)
  }

  it should "successfully transpile if a definition is not referenced" in {
    //there was a bug where we would try to index into the fields
    //at index -1 if a definition was never referenced

    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map("A" -> Right(JsonSchema.empty.copy(
          types = Seq(JsonSchemaInteger)
        )),
        "B" -> Right(JsonSchema.empty.copy(
          types = Seq(JsonSchemaBool)
        ))
      ),
      properties = Map("C" -> Right(JsonSchema.empty.copy(
        ref = Uri.parseOption("#/definitions/A")
      ))),
      required = Seq("C")
    )

    val Right(_) = Transpiler.transpile(Right(root), None)
  }

  it should "transpile inline oneOf as union" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        properties = Map(
          "foo" -> Right(JsonSchema.empty.copy(
            oneOf = Seq(
              Right(JsonSchema.empty.copy(types = Seq(JsonSchemaNumber))),
              Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))
            )
          ),
        )),
        required = Seq("foo")
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None,
        Seq(AvroField("foo", None, AvroUnion(Seq(AvroDouble, AvroString)), None, None))
      )

    avroSchema should be(expectedRecord)
  }

  it should "transpile inline non-required oneOf as union" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        properties = Map(
          "foo" -> Right(JsonSchema.empty.copy(
            oneOf = Seq(
              Right(JsonSchema.empty.copy(types = Seq(JsonSchemaNumber))),
              Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))
            )
          )),
        ),
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None,
        Seq(AvroField("foo", None, AvroUnion(Seq(AvroNull, AvroDouble, AvroString)), Some(ujson.Null), None))
      )

    avroSchema should be(expectedRecord)
  }

  it should "inline first referenced definition in a union too" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        definitions = Map(
          "A" -> Right(JsonSchema.empty.copy(
            properties = Map(
              "qux" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))
            ),
            required = Seq("qux")
          ))
        ),
        properties = Map(
          "foo" -> Right(JsonSchema.empty.copy(
            oneOf = Seq(
              Right(JsonSchema.empty.copy(ref =  Uri.parseOption("#/definitions/A"))),
              Right(JsonSchema.empty.copy(types = Seq(JsonSchemaBool)))
            )
          )),
        ),
        required = Seq("foo")
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None,
        Seq(AvroField("foo", None,
            AvroUnion(Seq(
              AvroRecord("A", None, None, Seq(AvroField("qux", None, AvroString, None, None))),
              AvroBool,
            )),
            None, None
        ))
      )

    avroSchema should be(expectedRecord)
  }

  it should "inline first referenced definition in an array type" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map(
        "Reference" -> Right(JsonSchema.empty.copy(
          properties = Map("id" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))),
          required = Seq("id")
        ))
      ),
      properties = Map(
        "Account" -> Right(JsonSchema.empty.copy(
          properties = Map(
            "subject" -> Right(JsonSchema.empty.copy(
              items = Seq(Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/Reference")))),
              types = Seq(JsonSchemaArray)
            ))
          ),
          required = Seq("subject")
        ))
      )
    )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(
          AvroField("Account", None,
            AvroRecord("Account", None, None, Seq(
              AvroField("subject", None,
                AvroArray(
                  AvroRecord("Reference", None, None, Seq(
                    AvroField("id", None, AvroString, None, None)
                  )
                )
              ), None, None)
            ))
          ,None, None)
        ))

    avroSchema should equal(expectedRecord)
  }

  it should "inline first referenced definition in an nested array type" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map(
        "Reference" -> Right(JsonSchema.empty.copy(
          properties = Map("id" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))),
          required = Seq("id")
        )),
        "Account" -> Right(JsonSchema.empty.copy(
          properties = Map(
            "subject" -> Right(JsonSchema.empty.copy(
              items = Seq(Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/Reference")))),
              types = Seq(JsonSchemaArray)
            ))
          ),
          required = Seq("subject")
        ))
      ),
      oneOf = Seq(
        Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/Account"))),
        Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))
      )
    )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(
        AvroField("value", None,
          AvroUnion(Seq(
            AvroRecord("Account", None, None, Seq(
              AvroField("subject", None,
                AvroArray(
                  AvroRecord("Reference", None, None, Seq(
                    AvroField("id", None, AvroString, None, None)
                  ))
                ), None, None)
            )),
            AvroString,
          ))
          ,None, None)
      ))

    avroSchema should equal(expectedRecord)
  }

  it should "properly inline defs given a complex graph of self and circular references" in {
    val extension = Right(JsonSchema.empty.copy(
      types = Seq(JsonSchemaArray),
      items = Seq(Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/Extension"))))
    ))

    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map(
        "Element" -> Right(JsonSchema.empty.copy(
          properties = Map("extension" -> extension),
          required = Seq("extension")
        )),
        "Extension" -> Right(JsonSchema.empty.copy(
          properties = Map(
            "extension" -> extension,
            "elem" -> Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/Element")))
          ),
          required = Seq("extension", "elem")
        )),
        "Account" -> Right(JsonSchema.empty.copy(
          properties = Map(
            "_language" -> Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/Element"))),
            "extension" -> extension
          ),
          required = Seq("_language", "extension")
        ))
      ),
      oneOf = Seq(
        Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/Account"))),
        Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))
      )
    )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(
        AvroField("value", None,
          AvroUnion(Seq(
            AvroRecord("Account", None, None, Seq(
              AvroField("_language", None,
                AvroRecord("Element", None, None, Seq(
                  AvroField("extension", None,
                    AvroArray(AvroRecord("Extension", None, None, Seq(
                      AvroField("extension", None, AvroArray(AvroRef("Extension")), None, None),
                      AvroField("elem", None, AvroRef("Element"), None, None)
                    ))),
                    None, None)
                )),
                None, None),
              AvroField("extension", None, AvroArray(AvroRef("Extension")), None, None)
            )),
            AvroString,
          ))
          ,None, None)
      ))

    avroSchema should be(expectedRecord)
  }

  it should "transpile oneOf at root" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        oneOf = Seq(
          Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString))),
          Right(JsonSchema.empty.copy(types = Seq(JsonSchemaNumber)))
        )
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None,
        Seq(
          AvroField("value", None, AvroUnion(Seq(AvroString, AvroDouble)), None, None)
        )
      )

    avroSchema should be(expectedRecord)
  }

  it should "ignore boolean schemas in additionalProperties" in {
    /*
      Validation with "additionalProperties" applies only to the child
      values of instance names that do not match any names in "properties",
      and do not match any regular expression in "patternProperties".

      https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.20
     */
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        additionalProperties = Some(Left(false))
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq())

    avroSchema should be(expectedRecord)
  }

  it should "handle boolean schemas nested in other schemas" in {
    val root =
      JsonSchema.empty.copy(
        id = schemaUri,
        properties = Map("A" -> Right(JsonSchema.empty.copy(oneOf= Seq(Left(true))))),
        required = Seq("A")
      )

    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None,
        Seq(
          AvroField("A", None, AvroUnion(Seq(AvroBytes)), None, None)
        )
      )

    avroSchema should be(expectedRecord)
  }

  it should "sanitize names of boolean" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map("boolean" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaBool)))),
      properties = Map("A" -> Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/boolean")))),
      required = Seq("A")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(
        AvroField("A", None,
          AvroRecord("schemaBoolean", None, None, Seq(AvroField("value", None, AvroBool, None, None)))
          , None, None)
      ))

    avroSchema should be(expectedRecord)
  }

  it should "sanitize names of string" in {
    val root = JsonSchema.empty.copy(
      id = schemaUri,
      definitions = Map("string" -> Right(JsonSchema.empty.copy(types = Seq(JsonSchemaString)))),
      properties = Map("A" -> Right(JsonSchema.empty.copy(ref = Uri.parseOption("#/definitions/string")))),
      required = Seq("A")
    )
    val Right(avroSchema) = Transpiler.transpile(Right(root), None)

    val expectedRecord =
      AvroRecord("schema", None, None, Seq(
        AvroField("A", None,
          AvroRecord("schemaString", None, None, Seq(AvroField("value", None, AvroString, None, None)))
          , None, None)
      ))

    avroSchema should be(expectedRecord)
  }

  private def schemaUri =
    Uri.parseOption("http://json-schema.org/draft-06/schema#")
}
