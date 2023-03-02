package test.fastparse

import fastparse._
import utest._

/**
  * Same as MathTests, but demonstrating the use of whitespace
  */
object CustomWhitespaceMathTests extends TestSuite{
  implicit val whitespace: ParsingRun[_] => P[Unit] = { implicit ctx: ParsingRun[_] =>
    CharsWhileIn(" \t", 0)
  }
  def eval(tree: (Int, Seq[(String, Int)])): Int = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right case "-" => left - right
      case "*" => left * right case "/" => left / right
    }}
  }
  def number[$: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )
  def parens[$: P]: P[Int] = P( "(" ~/ addSub ~ ")" )
  def factor[$: P]: P[Int] = P( number | parens )

  def divMul[$: P]: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  def addSub[$: P]: P[Int] = P( divMul ~ (CharIn("+\\-").! ~/ divMul).rep ).map(eval)
  def expr[$: P]: P[Int]   = P( " ".rep ~ addSub ~ " ".rep ~ End )

  val tests = Tests {
    test("pass"){
      def check(str: String, num: Int) = {
        val Parsed.Success(value, _) = parse(str, expr(_))
        assert(value == num)
      }

      test - check("1+1", 2)
      test - check("1+   1*   2", 3)
      test - check("(1+   1  *  2)+(   3*4*5)", 63)
      test - check("15/3", 5)
      test - check("63  /3", 21)
      test - check("(1+    1*2)+(3      *4*5)/20", 6)
      test - check("((1+      1*2)+(3*4*5))/3", 21)
    }
    test("fail"){
      def check(input: String, expectedTrace: String) = {
        val failure =  parse(input, expr(_)).asInstanceOf[Parsed.Failure]
        val actualTrace = failure.trace().longAggregateMsg
        assert(expectedTrace.trim == actualTrace.trim)
      }
      test - check(
        "(  +  )",
        """ Expected expr:1:1 / addSub:1:1 / divMul:1:1 / factor:1:1 / parens:1:1 / addSub:1:4 / divMul:1:4 / factor:1:4 / (number | parens):1:4, found "+  )" """
      )
      test - check(
        "1  +  - ",
        """ Expected expr:1:1 / addSub:1:1 / divMul:1:7 / factor:1:7 / (number | parens):1:7, found "- " """
      )
    }
  }
}
