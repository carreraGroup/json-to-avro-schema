package io.carrera.jsontoavroschema

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class IntegrationTests extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers {

  private val testFiles = "src/test/resources/integration-tests/"

  private val tests = Table(
    "name",
    "null",
    "boolean",
    "double",
    "integer",
    "string",
    "emptySchema",
    "stringArray",
    "anyArray",
    "stringMap",
    "nested",
    "stringEnum",
    "union",
    "optionalBoolean",
    "siblingRef",
    "siblingRefWithId",
    "definitions",
    "oneof",
    "array-ref-def",
    "nestedUnionRefs",
    "selfref",
  )

  it should "run integration tests" in forAll(tests) { name =>
    val Right(avro) = Application.run(s"$testFiles$name.json", Some("com.example"))
    val goldenCopy = Application.loadFile(s"$testFiles$name.avsc").get
    ujson.read(avro) should be(ujson.read(ujson.read(goldenCopy)))
  }
}
