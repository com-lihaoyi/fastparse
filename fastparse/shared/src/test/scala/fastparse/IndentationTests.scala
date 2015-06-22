package fastparse

import fastparse.Implicits.Sequencer
import fastparse.core.Parser
import fastparse.parsers.Combinators.Sequence
import utest._

/**
 * Same as MathTests, but demonstrating the use of whitespace
 */
object IndentationTests extends TestSuite{
  def eval(tree: (String, Seq[Int])) = tree match{
    case ("+", nums) => nums.reduceLeft(_+_)
    case ("-", nums) => nums.reduceLeft(_-_)
    case ("*", nums) => nums.reduceLeft(_*_)
    case ("/", nums) => nums.reduceLeft(_/_)
  }

  class Parser(indent: Int){
    val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
    val factor: P[Int] = P( " ".rep(indent) ~ (number | op) )
    val indented: P[Int] = P( new Parser(indent+2).factor )
    val op: P[Int] = P( CharIn("+-*/").! ~! ("\n" ~ indented).rep(1) ).map(eval)
    val expr: P[Int]   = P( op ~ End )
  }
  val expr = new Parser(0).expr
  val tests = TestSuite{
    'pass {
      def check(str: String, num: Int) = {
        val Result.Success(value, _) = expr.parse(str)
        assert(value == num)
      }

      check(
        """+
          |  1
          |  1
        """.stripMargin.trim,
        2
      )
      check(
        """+
          |  1
          |  *
          |    1
          |    2
        """.stripMargin.trim,
        3
      )
      check(
        """+
          |  +
          |    1
          |    *
          |      1
          |      2
          |  *
          |    3
          |    4
          |    5
          |
        """.stripMargin.trim,
        63
      )
      check(
        """/
          |  15
          |  3
        """.stripMargin.trim,
        5
      )
      check(
        """/
          |  63
          |  3
        """.stripMargin.trim,
        21
      )
      check(
        """+
          |  +
          |    1
          |    *
          |      1
          |      2
          |  /
          |    *
          |      3
          |      4
          |      5
          |    20
        """.stripMargin.trim,
        6
      )
      check(
        """/
          |  +
          |    +
          |      1
          |      *
          |        1
          |        2
          |      *
          |        3
          |        4
          |        5
          |  3
        """.stripMargin.trim,
        21
      )
    }
    'fail{
      def check(input: String, trace: String) = {
        val failure = expr.parse(input, trace = false).asInstanceOf[Result.Failure]
        assert(trace == failure.trace)
      }
      check(
        "+",
        """"\n":1 ..."""""
      )
      check(
        """+
          |  1
          |1
        """.stripMargin.trim,
        """End:5 ..."\n1""""
      )
    }
  }

}
