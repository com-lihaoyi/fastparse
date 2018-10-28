package cssparse

import fastparse._

object TestUtil {

  def checkParsing(input: String, tag: String = "") = {

    def checkParsed(input: String, res: Parsed[Ast.RuleList]) = {
      res match {
        case f: Parsed.Failure =>
          throw new Exception(tag + "\n" + input + "\n" + f.trace().longAggregateMsg)
        case s: Parsed.Success[Ast.RuleList] =>
          val inputLength = input.length
          val index = s.index
          utest.assert(index == inputLength)
      }
    }

    val res = parse(input, CssRulesParser.ruleList(_))
    checkParsed(input, res)

    val parsedInput = PrettyPrinter.printRuleList(res.get.value)
    val res2 = parse(parsedInput, CssRulesParser.ruleList(_))
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
      errors.toSeq
    }

    val parsedInput = PrettyPrinter.printRuleList(parse(input, CssRulesParser.ruleList(_)).get.value)
    val initialErrors = getErrors(input)
    val parsingErrors = getErrors(parsedInput)

    assert(initialErrors.sorted == parsingErrors.sorted)
  }
}
