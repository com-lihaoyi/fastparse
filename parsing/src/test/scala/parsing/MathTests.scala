package parsing

import utest._

/**
 * Demonstrates simulatneously parsing and
 * evaluating simple arithmetic expressions
 */
object MathTests extends TestSuite{
  def eval(tree: (Int, Seq[(String, Int)])) = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => left / right
    }}
  }
  val number = R( CharIn('0'to'9').rep1.!.map(_.toInt) )
  val parens = R( "(" ~ addSub ~ ")" )
  val factor = R( number | parens )

  val divMul: R[Int] = R( factor ~ (CharIn("*/").! ~ factor).rep ).map(eval)
  val addSub: R[Int] = R( divMul ~ (CharIn("+-").! ~ divMul).rep ).map(eval)
  val expr = R( addSub ~ End )
  val tests = TestSuite{
    'pass {
      def check(str: String, num: Int) = {
        for (parser <- Seq(expr, EitherSequenceWalker.recurse(RuleWalker.recurse(expr, Nil), Nil))) {
          val res = parser.parse(str)
          assert(res == Result.Success(num, str.length))
        }
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
  println("    A")
  println(expr)
  println("    B")
  println(RuleWalker.recurse(expr, Nil))
  println("    C")
  println(EitherSequenceWalker.recurse(RuleWalker.recurse(expr, Nil), Nil))
}
