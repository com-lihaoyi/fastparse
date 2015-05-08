package fastparse

import fastparse.Result
import utest._

/**
 * Demonstrates simulatneously parsing and
 * evaluating simple arithmetic expressions
 */
object MathTests extends TestSuite{
  def eval(tree: (Int, Seq[(String, Int)])) = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right  case "-" => left - right
      case "*" => left * right case "/" => left / right
    }}
  }

  val number: R[Int] = R( CharIn('0'to'9').rep1.!.map(_.toInt) )
  val parens: R[Int] = R( "(" ~ addSub ~ ")" )
  val factor: R[Int] = R( number | parens )

  val divMul: R[Int] = R( factor ~ (CharIn("*/").! ~ factor).rep ).map(eval)
  val addSub: R[Int] = R( divMul ~ (CharIn("+-").! ~ divMul).rep ).map(eval)
  val expr: R[Int]   = R( addSub ~ End )

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

  }
//  println("    A")
//  println(expr)
//  println("    B")
//  println(RuleWalker.recurse(expr, Nil))
//  println("    C")
//  println(EitherSequenceWalker.recurse(RuleWalker.recurse(expr, Nil), Nil))
}
