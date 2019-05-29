package io.circe.yaml

import io.circe.Json
import org.scalatest.{FlatSpec, Matchers}

class NumberTests extends FlatSpec with Matchers {
  val printer = Printer.spaces2.copy(preserveOrder = true)
  "Printer" should "print integers as expected" in {
    import Json._
    val json = Json.obj(
      "string" -> fromString("x"),
      "integer0" -> fromInt(0),
      "integer10" -> fromInt(10),
      "integer10000" -> fromInt(100000)
    )
    val output = printer.pretty(json)
    assert(!(output contains "!!"))
  }

  it should "print doubles as expected" in {
    import Json._
    val json = Json.obj(
      "string" -> fromString("x"),
      "double0" -> fromDoubleOrNull(0),
      "double10" -> fromDoubleOrNull(10),
      "double10000" -> fromDoubleOrNull(100000),
      "double0p" -> fromDoubleOrNull(0.1234),
      "double0pp" -> fromDoubleOrNull(0.00001234),
      "double0ppp" -> fromDoubleOrNull(0.000000000000000000000000000000001234568)
    )
    val output = printer.pretty(json)
    assert(!(output contains "!!"))
  }
}
