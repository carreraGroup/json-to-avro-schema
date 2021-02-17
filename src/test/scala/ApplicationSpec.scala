package io.carrera.jsontoavroschema

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import ujson.Str

import scala.util.Success

class ApplicationSpec extends AnyFlatSpec {

  it should "get input file name" in {
    val args = Array("filePath")
    val Some(actual)  = Application.getInputFilePath(args)
    actual should be("filePath")
  }

  it should "loadFile" in {
    val expected =
      """{  "key": "value"}"""
    val Success(actual) = Application
      .loadFile("src/test/resources/basic.json")

    actual should be(expected)
  }

  it should "read to JSON" in {
    val actual = Application.readJson("""{"key": "value"}""")
    actual("key") should be(Str("value"))
  }
}
