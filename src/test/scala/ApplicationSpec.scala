package io.carrera.jsontoavroschema

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import ujson.Str

import scala.util.Success

class ApplicationSpec extends AnyFlatSpec {

  it should "parse args for input file" in {
    val args = List("filePath")
    val Right(options) = Application.parseArgs(args)
    options should be(Map("inputFile" -> "filePath"))
  }

  it should "require an inputFile" in {
    val args = List()
    val Left(message) = Application.parseArgs(args)
    message should be("inputFile is required")
  }

  it should "parse optional namespace" in {
    val args = List("--namespace", "com.example", "filePath")
    val Right(options) = Application.parseArgs(args)
    val expected = Map(
      "inputFile" -> "filePath",
      "namespace" -> "com.example"
    )
    options should be(expected)
  }

  it should "parse optional short flag namespace" in {
    val args = List("-n", "com.example", "filePath")
    val Right(options) = Application.parseArgs(args)
    val expected = Map(
      "inputFile" -> "filePath",
      "namespace" -> "com.example"
    )
    options should be(expected)
  }

  it should "require options before args" in {
    val args = List("filePath", "-n", "com.example")
    val Left(message) = Application.parseArgs(args)
    message should be("unrecognized option: filePath")
  }

  it should "loadFile" in {
    val expected =
      """{
        |  "key": "value"
        |}""".stripMargin
    val Success(actual) = Application
      .loadFile("src/test/resources/basic.json")

    actual should be(expected)
  }

  it should "read to JSON" in {
    val actual = Application.readJson("""{"key": "value"}""")
    actual("key") should be(Str("value"))
  }

  it should "successfully tranpsile reference schema" in {
    val Right(avroJson) = Application.run("src/test/resources/integration-test.json", Some("com.example"))
    val goldenCopy = Application.loadFile("src/test/resources/integration-test.avsc").get
    ujson.read(avroJson) should be(ujson.read(ujson.read(goldenCopy)))
  }
}
