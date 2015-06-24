package fastparse

import utest._
import all._
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

  /**
   * Parser for an indentation-based math syntax. Parens are no longer
   * necessary, and the whole parser is parametrized with the current
   * depth of indentation
   */
  class Parser(indent: Int){
    val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
    val deeper: P[Int] = P( " ".rep(indent + 1).!.map(_.length) )
    val factor: P[Int] = P( number | block )
    val blockBody: P[Seq[Int]] = "\n" ~ deeper.flatMap(i =>
      new Parser(i).factor.rep(1, sep = ("\n" + " " * i) ~!)
    )
    val block: P[Int] = P( CharIn("+-*/").! ~! blockBody).map(eval)
    val expr: P[Int]   = P( block ~ End )
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
      check(
        """+
          |  1
          |   1
        """.stripMargin.trim,
        """(number | block):8 ..." 1""""
      )
    }
  }

}
