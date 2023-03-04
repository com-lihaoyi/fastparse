package test.fastparse

import utest._
import fastparse._
import fastparse.internal.Util

/**
  * Same as MathTests, but demonstrating the use of whitespace
  */
object IndentationTests extends TestSuite{
  import fastparse.NoWhitespace._
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
    def number[$: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )

    def deeper[$: P]: P[Int] = P( " ".rep(indent + 1).!.map(_.length) )
    def blockBody[$: P]: P[Seq[Int]] = "\n" ~ deeper.flatMapX(i =>
      new Parser(indent = i).factor.rep(1, sep = ("\n" + " " * i)./)
    )
    def block[$: P]: P[Int] = P( CharIn("+\\-*/").! ~/ blockBody).map(eval)

    def factor[$: P]: P[Int] = P( number | block )

    def expr[$: P]: P[Int]   = P( block ~ End )
  }
  def expr[$: P] = new Parser(indent = 0).expr

  val tests = Tests {
    test("pass"){
      def check(str: String, num: Int) = {
        val Parsed.Success(value, _) = parse(str, expr(_))
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
    test("fail"){
      def check(input: String, expectedTrace: String): Unit = {
        val failure = parse(input, expr(_)).asInstanceOf[Parsed.Failure]
        val actualTrace = failure.trace(enableLogging = true).longreportParseMsg
        assert(expectedTrace.trim == actualTrace.trim)
      }
      test - check(
        "+",
        """ Expected expr:1:1 / block:1:1 / "\n":1:2, found "" """
      )
      test - check(
        """+
          |  1
          |1
        """.stripMargin.trim,
        """ Expected expr:1:1 / ([0-9] | "\n  " | end-of-input):2:4, found "\n1" """
      )
      test - check(
        """+
          |  1
          |   1
        """.stripMargin.trim,
        """ Expected expr:1:1 / block:1:1 / factor:3:3 / (number | block):3:3, found " 1" """
      )
    }
  }
}
