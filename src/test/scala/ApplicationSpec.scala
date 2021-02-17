package io.carrera.jsontoavroschema

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ApplicationSpec extends AnyFlatSpec {

  it should "cube it" in {
    Application.cube(3) should be(27)
  }
}
