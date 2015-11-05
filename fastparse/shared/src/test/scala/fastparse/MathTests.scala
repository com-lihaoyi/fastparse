package fastparse
import all._
import fastparse.core.Result
import utest._

/**
 * Demonstrates simultaneously parsing and
 * evaluating simple arithmetic expressions
 */
object MathTests extends TestSuite{
  def eval(tree: (Int, Seq[(String, Int)])) = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right case "-" => left - right
      case "*" => left * right case "/" => left / right
    }}
  }

  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
  val parens: P[Int] = P( "(" ~!~ addSub ~ ")" )
  val factor: P[Int] = P( number | parens )

  val divMul: P[Int] = P( factor ~ (CharIn("*/").! ~!~ factor).rep ).map(eval)
  val addSub: P[Int] = P( divMul ~ (CharIn("+-").! ~!~ divMul).rep ).map(eval)
  val expr: P[Int]   = P( addSub ~ End )

  val tests = TestSuite{
    'pass {
      def check(str: String, num: Int) = {
        val Result.Success(value, _) = expr.parse(str)
        assert(value == num)
      }

      check("1+1", 2)
      check("1+1*2", 3)
      check("(1+1*2)+(3*4*5)", 63)
      check("15/3", 5)
      check("63/3", 21)
      check("(1+1*2)+(3*4*5)/20", 6)
      check("((1+1*2)+(3*4*5))/3", 21)
    }
    'fail{
      def check(input: String, expectedTrace: String) = {
        val failure = expr.parse(input).asInstanceOf[Result.Failure]
        val actualTrace = failure.traced.trace
        assert(expectedTrace.trim == actualTrace.trim)
      }
      check(
        "(+)",
        """expr:0 / addSub:0 / divMul:0 / factor:0 / parens:0 / addSub:1""" +
        """ / divMul:1 / factor:1 / (number | parens):1 ..."+)" """
      )
      check(
        "1+-",
        """ expr:0 / addSub:0 / divMul:2 / factor:2 / (number | parens):2 ..."-" """
      )
      check(
        "(1+(2+3x))+4",
        """ expr:0 / addSub:0 / divMul:0 / factor:0 / parens:0 / addSub:1""" +
        """ / divMul:3 / factor:3 / parens:3 / (")" | CharIn("+-")):7 ..."x))+4" """
      )
    }
  }

}
