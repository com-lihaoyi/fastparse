package cssparse

import fastparse.all._

object TestUtil {

  def checkParsing(input: String, tag: String = "") = {

    def checkParsed(input: String, res: Parsed[Ast.RuleList]) = {
      res match {
        case f: Parsed.Failure =>
          throw new Exception(tag + "\n" + input + "\n" + f.extra.traced.trace)
        case s: Parsed.Success[Ast.RuleList] =>
          val inputLength = input.length
          val index = s.index
          assert(index == inputLength)
      }
    }

    val res = CssRulesParser.ruleList.parse(input)
    checkParsed(input, res)

    val parsedInput = PrettyPrinter.printRuleList(res.get.value)
    val res2 = CssRulesParser.ruleList.parse(parsedInput)
    checkParsed(parsedInput, res2)
  }


  def checkPrinting(input: String, tag: String = "") = {
    import java.io.StringReader
    import org.w3c.css.sac.InputSource
    import org.w3c.css.sac._
    import com.steadystate.css.parser._
    import scala.collection.mutable.ArrayBuffer

    def getErrors(css: String): Seq[String] = {
      val errors = ArrayBuffer[String]()
      val source = new InputSource(new StringReader(css))
      val parser = new CSSOMParser(new SACParserCSS3())
      parser.setErrorHandler(new ErrorHandler{
        def error(ex: CSSParseException) = {
          errors += ex.toString
          println("ERROR " + ex + " Line: " + ex.getLineNumber + " Column:" + ex.getColumnNumber)
        }
        def fatalError(ex: CSSParseException) = {
          errors += ex.toString
          println("FATAL ERROR " + ex)
        }
        def warning(ex: CSSParseException) = println("WARNING " + ex)
      })
      val sheet = parser.parseStyleSheet(source, null, null)
      errors
    }

    val parsedInput = PrettyPrinter.printRuleList(CssRulesParser.ruleList.parse(input).get.value)
    val initialErrors = getErrors(input)
    val parsingErrors = getErrors(parsedInput)

    assert(initialErrors.sorted == parsingErrors.sorted)
  }
}
