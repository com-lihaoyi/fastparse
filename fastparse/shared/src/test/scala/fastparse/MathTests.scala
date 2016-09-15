package fastparse

import utest._

import scala.collection.mutable

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

  import fastparse.all._

  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
  val parens: P[Int] = P( "(" ~/ addSub ~ ")" )
  val factor: P[Int] = P( number | parens )

  val divMul: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  val addSub: P[Int] = P( divMul ~ (CharIn("+-").! ~/ divMul).rep ).map(eval)
  val expr: P[Int]   = P( addSub ~ End )

  val tests = TestSuite{
    'pass {
      val Parsed.Success(2, _) = expr.parse("1+1")
      val Parsed.Success(15, _) = expr.parse("(1+1*2)+3*4")
      val Parsed.Success(21, _) = expr.parse("((1+1*2)+(3*4*5))/3")
      val Parsed.Failure(expected, failIndex, extra) = expr.parse("1+1*")
      assert(expected == (number | parens), failIndex == 4)
    }
    'fail{
      def check(input: String, expectedTrace: String, expectedShortTrace: String) = {
        val failure = expr.parse(input).asInstanceOf[Parsed.Failure]
        val actualTrace = failure.extra.traced.trace
        assert(expectedTrace.trim == actualTrace.trim)

        // Check iterator parsing results in a failure in the right place. Note
        // that we aren't checking the `.traced.trace` because that requires a
        // second parse which doesn't work with iterators (which get exhausted)
        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
          val failure = expr.parseIterator(input.grouped(chunkSize)).asInstanceOf[Parsed.Failure]
          assert(failure.msg == expectedShortTrace.trim)
        }
      }
      check(
        "(+)",
        """expr:1:1 / addSub:1:1 / divMul:1:1 / factor:1:1 / parens:1:1 / addSub:1:2""" +
        """ / divMul:1:2 / factor:1:2 / (number | parens):1:2 ..."+)" """,
        """ (number | parens):1 ..."+)" """
      )
      check(
        "1+-",
        """ expr:1:1 / addSub:1:1 / divMul:1:3 / factor:1:3 / (number | parens):1:3 ..."-" """,
        """ (number | parens):2 ..."-" """
      )
      check(
        "(1+(2+3x))+4",
        """ expr:1:1 / addSub:1:1 / divMul:1:1 / factor:1:1 / parens:1:1 / addSub:1:2""" +
        """ / divMul:1:4 / factor:1:4 / parens:1:4 / (")" | CharIn("+-")):1:8 ..."x))+4" """,
        """ ")":7 ..."x))+4" """
      )
    }

    'instrument{
      'simple{
        val callCount = mutable.Map.empty[String, Int]


        val instrumentFunction = (parser: Parser[_], index: Int, continuation: () => Parsed[_]) => {
          callCount(parser.toString) = callCount.getOrElse(parser.toString, 0) + 1
        }

        expr.parse("((1+1*2)+(3*4*5))/3", instrument = instrumentFunction)

        val expectedCallCount = Map(
          "expr" -> 1,
          "addSub" -> 4,
          "divMul" -> 6,
          "factor" -> 10,
          "number" -> 10,
          "parens" -> 3
        )
        assert(callCount == expectedCallCount)
      }
      'continuation{
        val resultCount = mutable.Map.empty[(String, Boolean), Int]
        val instrumentFunction = (parser: Parser[_], index: Int, continuation: () => Parsed[_]) => {
          val result = continuation()
          val resultKey = (parser.toString, result.isInstanceOf[Parsed.Success[_]])
          resultCount(resultKey) = resultCount.getOrElse(resultKey, 0) + 1
        }

        // Good Parse
        expr.parse("((1+1*2)+(3*4*5))/3", instrument = instrumentFunction)

        val expectedResultCount = Map(
          ("expr", true) -> 1,
          ("addSub", true) -> 4,
          ("divMul", true) -> 6,
          ("factor", true) -> 10,
          ("number", true) -> 7,
          ("number", false) -> 3,
          ("parens", true) -> 3
        )
        assert(resultCount == expectedResultCount)

        // Bad Parse
        resultCount.clear()
        expr.parse("((1+1*2)+(3*4*))/3", instrument = instrumentFunction)

        val expectedResultCount2 = Map(
          ("expr", false) -> 1,

          ("addSub", true) -> 1,
          ("addSub", false) -> 3,

          ("divMul", true) -> 3,
          ("divMul", false) -> 3,

          ("factor", true) -> 6,
          ("factor", false) -> 3,

          ("number", true) -> 5,
          ("number", false) -> 4,

          ("parens", true) -> 1,
          ("parens", false) -> 3
        )
        assert(resultCount == expectedResultCount2)
      }

    }
  }

}
