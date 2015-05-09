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
    helper(container, scalaparser.Scala.CompilationUnit, "((5/2)+1*3)")
  }
  @JSExport
  def math(container: html.Div) = {
    helper(container, fastparse.MathTests.expr, "((5/2)+1*3)")
  }
  @JSExport
  def json(container: html.Div) = {
    helper(container, fastparse.JsonTests.jsonExpr,  """
            {
                "firstName": "John",
                "lastName": "Smith",
                "age": 25,
                "address": {
                    "streetAddress": "21 2nd Street",
                    "city": "New York",
                    "state": "NY",
                    "postalCode": 10021
                },
                "phoneNumbers": [
                    {
                        "type": "home",
                        "number": "212 555-1234"
                    },
                    {
                        "type": "fax",
                        "number": "646 555-4567"
                    }
                ]
            }
    """)
  }
  def helper(container: html.Div, parser: Parser[_], default: String) = {
    import scalatags.JsDom.all._
    val inputBox = textarea(width:="45%", float.left, default).render
    val outputBox = div(width:="45%", float.right).render

    def recalc() = {
      val details = parser.parse(inputBox.value) match{
        case s: fastparse.Result.Success[_] =>
          table(
            width := "100%",
            tr(td("Success!")),
            tr(td("value:"), td(code(s.value.toString)))
          )

        case f: fastparse.Result.Failure =>
          val pretty = fastparse.Utils.literalize(f.input.slice(f.index, 50)).toString
          table(
            width := "100%",
            tr(td("Failure!")),
            tr(td("at index:"), td(code(f.index))),
            tr(td("found:"), td(code(pretty))),
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
