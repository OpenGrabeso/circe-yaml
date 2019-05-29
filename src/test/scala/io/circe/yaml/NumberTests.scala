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

  "Json Parser" should "parse JSON doubles as doubles" in {
    val input =
      """
        |{
        |  "x1": "0.1",
        |  "x2": "0.20000000298023224",
        |  "x3": "0.12345678901234567"
        |}
      """.stripMargin
    import io.circe.parser._
    val json = decode[Json](input).right.get
    val output = printer.pretty(json)
    assert(!(output contains "!!"))
  }

  "Encoder" should "encode doubles as doubles" in {
    case class XY(x: Double, y: Double)

    val xy = XY(1.0/3, 1.0/7)
    import io.circe.syntax._
    import io.circe.generic.auto._
    val json = xy.asJson
    val output = printer.pretty(json)
    assert(!(output contains "!!"))
  }
}
