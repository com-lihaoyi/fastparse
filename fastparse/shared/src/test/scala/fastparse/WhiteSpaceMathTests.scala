package fastparse

import acyclic.file
import utest._

/**
 * Same as MathTests, but demonstrating the use of whitespace
 */
object WhiteSpaceMathTests extends TestSuite{
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._
  def eval(tree: (Int, Seq[(String, Int)])): Int = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right case "-" => left - right
      case "*" => left * right case "/" => left / right
    }}
  }
  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
  val parens: P[Int] = P( "(" ~! addSub ~ ")" )
  val factor: P[Int] = P( number | parens )

  val divMul: P[Int] = P( factor ~ (CharIn("*/").! ~! factor).rep ).map(eval)
  val addSub: P[Int] = P( divMul ~ (CharIn("+-").! ~! divMul).rep ).map(eval)
  val expr: P[Int]   = P( " ".rep ~ addSub ~ " ".rep ~ End )

  val tests = TestSuite{
    'pass {
      def check(str: CharSequence, num: Int) = {
        val Result.Success(value, _) = expr.parse(str)
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
    'fail{
      def check(input: CharSequence, expectedTrace: String) = {
        val failure = expr.parse(input).asInstanceOf[Result.Failure]
        val actualTrace = failure.traced.trace
        assert(expectedTrace.trim == actualTrace.trim)
      }
      * - check(
        "(  +  )",
        """ expr:0 / addSub:0 / divMul:0 / factor:0 / parens:0 / addSub:3 / divMul:3 / factor:3 / (number | parens):3 ..."+  )" """
      )
      * - check(
        "1  +  - ",
        """ expr:0 / addSub:0 / divMul:6 / factor:6 / (number | parens):6 ..."- " """
      )
    }
  }

}
