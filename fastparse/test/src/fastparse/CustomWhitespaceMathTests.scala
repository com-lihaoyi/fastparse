package fastparse

import fastparse._
import utest._

/**
  * Same as MathTests, but demonstrating the use of whitespace
  */
object CustomWhitespaceMathTests extends TestSuite{
  implicit val whitespace = { implicit ctx: ParsingRun[_] =>
    CharsWhileIn(" \t", 0)
  }
  def eval(tree: (Int, Seq[(String, Int)])): Int = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right case "-" => left - right
      case "*" => left * right case "/" => left / right
    }}
  }
  def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )
  def parens[_: P]: P[Int] = P( "(" ~/ addSub ~ ")" )
  def factor[_: P]: P[Int] = P( number | parens )

  def divMul[_: P]: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  def addSub[_: P]: P[Int] = P( divMul ~ (CharIn("+\\-").! ~/ divMul).rep ).map(eval)
  def expr[_: P]: P[Int]   = P( " ".rep ~ addSub ~ " ".rep ~ End )

  val tests = Tests {
    'pass - {
      def check(str: String, num: Int) = {
        val Parsed.Success(value, _) = parse(str, expr(_))
        assert(value == num)
      }

      * - check("1+1", 2)
      * - check("1+   1*   2", 3)
      * - check("(1+   1  *  2)+(   3*4*5)", 63)
      * - check("15/3", 5)
      * - check("63  /3", 21)
      * - check("(1+    1*2)+(3      *4*5)/20", 6)
      * - check("((1+      1*2)+(3*4*5))/3", 21)
    }
    'fail - {
      def check(input: String, expectedTrace: String) = {
        val failure =  parse(input, expr(_)).asInstanceOf[Parsed.Failure]
        val actualTrace = failure.trace().longAggregateMsg
        assert(expectedTrace.trim == actualTrace.trim)
      }
      * - check(
        "(  +  )",
        """ Expected expr:1:1 / addSub:1:1 / divMul:1:1 / factor:1:1 / parens:1:1 / addSub:1:4 / divMul:1:4 / factor:1:4 / (number | parens):1:4, found "+  )" """
      )
      * - check(
        "1  +  - ",
        """ Expected expr:1:1 / addSub:1:1 / divMul:1:7 / factor:1:7 / (number | parens):1:7, found "- " """
      )
    }
  }
}