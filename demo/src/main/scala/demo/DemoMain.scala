package demo

import fastparse.Parser
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

@JSExport
object DemoMain {
  @JSExport
  def scalaparse(container: html.Div) = {
    val example =
      """
        |package scalaparser
        |
        |/**
        | * Created by haoyi on 4/26/15.
        | */
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    println("Hello World!")
        |  }
        |}""".stripMargin
    helper(container, scalaparser.Scala.CompilationUnit, example)
  }
  @JSExport
  def math(container: html.Div) = {
    helper(container, fastparse.MathTests.expr, "((1+1*2)+(3*4*5))/3")
  }
  @JSExport
  def json(container: html.Div) = {
    helper(container, fastparse.JsonTests.jsonExpr,
      """{
        |  "firstName": "John",
        |  "lastName": "Smith",
        |  "age": 25,
        |  "address": {
        |      "streetAddress": "21 2nd Street",
        |      "city": "New York",
        |      "state": "NY",
        |      "postalCode": 10021
        |  },
        |  "phoneNumbers": [
        |      {
        |          "type": "home",
        |          "number": "212 555-1234"
        |      },
        |      {
        |          "type": "fax",
        |          "number": "646 555-4567"
        |      }
        |  ]
        |}""".stripMargin)
  }
  def helper(container: html.Div, parser: Parser[_], default: String) = {
    import scalatags.JsDom.all._
    val inputBox = textarea(
      width := "45%",
      float.left,
      fontFamily := "monospace",
      default
    ).render

    val outputBox = div(width:="45%", float.right).render

    def recalc() = {
      inputBox.rows = inputBox.value.lines.length
      val details = parser.parse(inputBox.value) match{
        case s: fastparse.Result.Success[_] =>
          table(
            width := "100%",
            tr(td("Success!")),
            tr(td("value:"), td(code(s.value.toString)))
          )

        case f: fastparse.Result.Failure =>
          val pretty = fastparse.Utils.literalize(f.input.slice(f.index, f.index + 15)).toString
          table(
            width := "100%",
            tr(td("Failure!")),
            tr(td("at index:"), td(code(f.index))),
            tr(td("found:"), td("...", code(pretty))),
            tr(td("expected:"), td(code(f.parser.toString)))
          )
      }
      outputBox.innerHTML = ""
      outputBox.appendChild(details.render)
    }
    recalc()
    inputBox.onkeyup = (e: dom.Event) => recalc()

    container.appendChild(div(inputBox, outputBox, div(clear.both)).render)
  }
}
