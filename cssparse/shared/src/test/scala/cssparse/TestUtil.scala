package cssparse

import fastparse.all._
import utest.asserts.assert

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
}
