package test.fasterparser

import utest._
import fasterparser._, Parsing._
/**
  * Same as MathTests, but demonstrating the use of whitespace
  */
object IndentationTests extends TestSuite{
  implicit def whitespace(cfg: Parse[_]): Parse[Unit] = Pass(cfg)
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
    def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )

    def deeper[_: P]: P[Int] = P( " ".rep(indent + 1).!.map(_.length) )
    def blockBody[_: P]: P[Seq[Int]] = "\n" ~ deeper.flatMap(i =>
      new Parser(indent = i).factor.rep(1, sep = ("\n" + " " * i)./)
    )
    def block[_: P]: P[Int] = P( CharIn("+\\-*/").! ~/ blockBody).map(eval)

    def factor[_: P]: P[Int] = P( number | block )

    def expr[_: P]: P[Int]   = P( block ~ End )
  }
  def expr[_: P] = new Parser(indent = 0).expr
  val tests = Tests {
    'pass - {
      def check(str: String, num: Int) = {
        val Result.Success(value, _) = Parse(str).read(expr(_))
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
    'fail - {
      def check(input: String, expectedTrace: String) = {
        val failure = Parse(input).read(expr(_)).asInstanceOf[Result.Failure]
        val actualTrace = failure.extra.traced.trace
        assert(expectedTrace.trim == actualTrace.trim)
      }
      * - check(
        "+",
        """ Expected expr:1:1 / block:1:1 / "\n":1:2, found "" """
      )
      * - check(
        """+
          |  1
          |1
        """.stripMargin.trim,
        """ Expected expr:1:1 / [0-9] | "\n  " | end-of-input:2:4, found "\n1" """
      )
      * - check(
        """+
          |  1
          |   1
        """.stripMargin.trim,
        """ Expected expr:1:1 / block:1:1 / factor:3:3 / [0-9] | [+\-*/]:3:3, found " 1" """
      )
    }
  }

}