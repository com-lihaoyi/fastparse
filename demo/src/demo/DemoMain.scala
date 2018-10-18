package demo

import cssparse.PrettyPrinter
import org.scalajs.dom
import org.scalajs.dom.{Event, UIEvent, html}
import fastparse._
import fastparse.internal.Util

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

@JSExportTopLevel("demo.DemoMain")
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
    helper(container, scalaparse.Scala.CompilationUnit(_), example)
  }
  @JSExport
  def math(container: html.Div) = {
    helper(container, test.fastparse.MathTests.expr(_), "((1+1*2)+(3*4*5))/3")
  }
  @JSExport
  def whitespaceMath(container: html.Div) = {
    helper(container, test.fastparse.WhitespaceMathTests.expr(_), "  (  (  1+1  * 2   ) +( 3* 4  *5  )  )/3")
  }
  @JSExport
  def indentation(container: html.Div) = {
    helper(
      container,
      test.fastparse.IndentationTests.expr(_),
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
    helper(container, test.fastparse.Json.jsonExpr(_),
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
  @JSExport
  def css(container: html.Div) = {
   helper(container, cssparse.CssRulesParser.ruleList(_).map(PrettyPrinter.printRuleList(_)),
   """b,
     |strong {
     |  font-weight:         bold;
     |}
     |dfn
     |{
     |  font-style:   italic;
     |}
     |h1 {
     |  margin: .67em 0;
     |
     |
     |  font-size: 2em
     |}""".stripMargin)
  }
  @JSExport
  def python(container: html.Div) = {
   helper(container, pythonparse.Statements.file_input(_),
   """def foo(x, y):
     |  return x - y
     |""".stripMargin)
  }

  def helper(container: html.Div, parser: P[_] => P[Any], default: String) = {
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
      val details = parse(inputBox.value, parser) match{
        case s: fastparse.Parsed.Success[_] =>
          table(
            width := "100%",
            tr(td("Success!")),
            tr(td("value:"), td(pre(s.value.toString)))
          )

        case fastparse.Parsed.Failure(stack, index, extra) =>
          val pretty = Util.literalize( extra.input.slice( index, index + 15)).toString
          table(
            width := "100%",
            tr(td("Failure!")),
            tr(td("at index:"), td(code(index))),
            tr(td("found:"), td("...", code(pretty))),
            tr(td("expected:"), td(code(extra.trace().label)))
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
