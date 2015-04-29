package parsing

import utest._

object MathTests extends TestSuite{
  val number = R( CharSets('0'to'9').rep1.!.map(_.toInt) )
  val parens = R( "(" ~ addSub ~ ")" )
  val factor = R( number | parens )

  val divMul: R[Int] = R( factor ~ (CharSets("*/").! ~ factor).rep ).map{
    case (a, s) => s.foldLeft(a){
      case (left, ("*", right)) => left * right
      case (left, ("/", right)) => left / right
    }
  }
  val addSub: R[Int] = R( divMul ~ (CharSets("+-").! ~ divMul).rep ).map{
    case (a, s) => s.foldLeft(a){
      case (left, ("+", right)) => left + right
      case (left, ("-", right)) => left - right
    }
  }
  val expr = R( addSub ~ End )
  val tests = TestSuite{
    'pass {
      def check(str: String, num: Int) = {
        val res = expr.parse(str)
        assert(res == Result.Success(num, str.length))
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
}
