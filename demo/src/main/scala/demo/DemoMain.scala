package demo

import fastparse.all._
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

@JSExport
object DemoMain {
  @JSExport
  def scalaparser(container: html.Div) = {
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
    helper(container, scalaparse.Scala.CompilationUnit, example)
  }
  @JSExport
  def math(container: html.Div) = {
    helper(container, fastparse.MathTests.expr, "((1+1*2)+(3*4*5))/3")
  }
  @JSExport
  def whitespaceMath(container: html.Div) = {
    helper(container, fastparse.WhiteSpaceMathTests.expr, "  (  (  1+1  * 2   ) +( 3* 4  *5  )  )/3")
  }
  @JSExport
  def indentation(container: html.Div) = {
    helper(
      container,
      fastparse.IndentationTests.expr,
      """+
        |  +
        |    1
        |    *
        |      1
        |      2
        |  *
        |    3
        |    4
        |    5""".stripMargin
    )
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
      fontSize := 16,
      default
    ).render

    val outputBox = div(width:="45%", float.right, overflowX.scroll).render

    def recalc() = {
      inputBox.rows = inputBox.value.lines.length
      val details = parser.parse(inputBox.value) match{
        case s: Parsed.Success[_] =>
          table(
            width := "100%",
            tr(td("Success!")),
            tr(td("value:"), td(code(s.value.toString)))
          )

        case Parsed.Failure(lastParser, index, extra) =>
          val pretty = fastparse.Utils.literalize( extra.input.slice( index, index + 15)).toString
          table(
            width := "100%",
            tr(td("Failure!")),
            tr(td("at index:"), td(code(index))),
            tr(td("found:"), td("...", code(pretty))),
            tr(td("expected:"), td(code(lastParser.toString)))
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
