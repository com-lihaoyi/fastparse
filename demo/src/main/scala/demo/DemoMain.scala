package demo

import classparse.ClassParse
import cssparse.PrettyPrinter
import org.scalajs.dom
import org.scalajs.dom.{Event, UIEvent, html}
import fastparse.core.{Parsed, Parser}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

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
  @JSExport
  def css(container: html.Div) = {
   import fastparse.all._
   helper(container, cssparse.CssRulesParser.ruleList.map(PrettyPrinter.printRuleList(_)),
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
  def bmp(container: html.Div) = {
    import fastparse.byte._
    import fastparse.BmpTests.BmpParse

    helperByteFile(container, BmpParse.bmp.map(bmp => {
      bmp.bitmapHeader match {
        case h: BmpParse.BmpAst.BitmapInfoHeader =>
          val p = h.infoPart
          s"""
            |Width: ${p.width}
            |Height: ${p.height}
            |Bit per pixel: ${p.bitsPerPixel}
            |Horizontal resolution ${p.horzRes}
            |Vertical resolution ${p.vertRes}
            |Image size ${p.imageSize}
          """.stripMargin
      }
    }))
  }
  @JSExport
  def clss(container: html.Div) = {
    import fastparse.byte._
    helperByteFile(container, classparse.ClassParse.classFile.map(c => {
      val ast = ClassParse.Ast.convertToAst(c)
      s"""
         |Fields:
         |${ast.fields.map(field => field.descriptor + " " + field.name).mkString("\n")}
         |
         |Methods:
         |${ast.methods.map(method => method.descriptor + " " + method.name).mkString("\n")}
       """.stripMargin
    }))
  }
  def helper(container: html.Div, parser: Parser[_, Char, String], default: String) = {
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
        case s: Parsed.Success[_, Char] =>
          table(
            width := "100%",
            tr(td("Success!")),
            tr(td("value:"), td(pre(s.value.toString)))
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

  def helperByteFile(container: html.Div, parser: Parser[_, Byte, Array[Byte]]) = {
    import scalatags.JsDom.all._
    val uploadFile = input(
      width := "30%",
      float.left,
      id := "uploadFile",
      tpe := "file",
      "Upload file"
    ).render

    val outputBox = div(width:="65%", float.right, overflowX.scroll).render

    def recalc(e: dom.Event) = {
      val reader = new dom.FileReader()
      reader.readAsArrayBuffer(uploadFile.files.item(0))
      reader.onload = (e: UIEvent) => {
        val array = new Uint8Array(reader.result.asInstanceOf[ArrayBuffer])
        val contents = (for (i <- 0 until array.length) yield array.get(i).toByte).toArray

        val details = parser.parse(contents) match {
          case s: Parsed.Success[_, Byte] =>
            table(
              width := "100%",
              tr(td("Success!")),
              tr(td("value:"), td(pre(s.value.toString)))
            )

          case Parsed.Failure(lastParser, index, extra) =>
            table(
              width := "100%",
              tr(td("Failure!")),
              tr(td("at index:"), td(code(index))),
              tr(td("expected:"), td(code(lastParser.toString)))
            )
        }
        outputBox.innerHTML = ""
        outputBox.appendChild(details.render)
      }
    }

    uploadFile.onchange = (e: dom.Event) => recalc(e)
    container.appendChild(div(uploadFile, outputBox, div(clear.both)).render)
  }
}
