package test.fastparse

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
  import fastparse._, NoWhitespace._
  def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )
  def parens[_: P]: P[Int] = P( "(" ~/ addSub ~ ")" )
  def factor[_: P]: P[Int] = P( number | parens )

  def divMul[_: P]: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  def addSub[_: P]: P[Int] = P( divMul ~ (CharIn("+\\-").! ~/ divMul).rep ).map(eval)
  def expr[_: P]: P[Int]   = P( addSub ~ End )

  val tests = Tests {
    'pass - {
      val Parsed.Success(2, _) = parse("1+1", expr(_))
      val Parsed.Success(15, _) = parse("(1+1*2)+3*4", expr(_))
      val Parsed.Success(21, _) = parse("((1+1*2)+(3*4*5))/3", expr(_))
      val Parsed.Failure(expected, failIndex, extra) = parse("1+1*", expr(_))
      assert(
        failIndex == 4,
        extra.traced.trace == """Expected expr:1:1 / addSub:1:1 / divMul:1:3 / factor:1:5 / ([0-9] | "("):1:5, found """""
      )
    }
    'fail - {
      def check(input: String, expectedTrace: String, expectedShortTrace: String) = {
        val failure = parse(input, expr(_)).asInstanceOf[Parsed.Failure]
        val actualTrace = failure.extra.traced.trace
        assert(expectedTrace.trim == actualTrace.trim)

        // Check iterator parsing results in a failure in the right place. Note
        // that we aren't checking the `.traced.trace` because that requires a
        // second parse which doesn't work with iterators (which get exhausted)
        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)) {
          val failure2 = parseIterator(input.grouped(chunkSize), expr(_)).asInstanceOf[Parsed.Failure]
          val trace = failure2.trace
          assert(trace == expectedShortTrace.trim)
        }
      }
      check(
        "(+)",
        """Expected expr:1:1 / addSub:1:1 / divMul:1:1 / factor:1:1 / parens:1:1 / addSub:1:2 / divMul:1:2 / factor:1:2 / ([0-9] | "("):1:2, found "+)"""",
        """Position 1, found "+)""""
      )
      check(
        "1+-",
        """Expected expr:1:1 / addSub:1:1 / divMul:1:3 / factor:1:3 / ([0-9] | "("):1:3, found "-"""",
        """Position 2, found "-""""
      )
      check(
        "(1+(2+3x))+4",
        """Expected expr:1:1 / addSub:1:1 / divMul:1:1 / factor:1:1 / parens:1:1 / addSub:1:2 / divMul:1:4 / factor:1:4 / parens:1:4 / ([0-9] | [*/] | [+\\-] | ")"):1:8, found "x))+4"""",
        """Expected ")":7, found "x))+4""""
      )
    }
    'instrument - {
      'simple - {
        val callCount = mutable.Map.empty[String, Int]


        val instrument = new ParsingRun.Instrument {
          def beforeParse(parser: String, index: Int): Unit = {
            callCount(parser) = callCount.getOrElse(parser, 0) + 1
          }
          def afterParse(parser: String, index: Int, success: Boolean): Unit = ()
        }

        parse("((1+1*2)+(3*4*5))/3", expr(_), instrument = instrument)

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
      'continuation - {
        val resultCount = mutable.Map.empty[(String, Boolean), Int]
        val instrument = new ParsingRun.Instrument {
          def beforeParse(parser: String, index: Int): Unit = ()
          def afterParse(parser: String, index: Int, success: Boolean): Unit = {
            val resultKey = (parser, success)
            resultCount(resultKey) = resultCount.getOrElse(resultKey, 0) + 1
          }
        }

        // Good Parse
        parse("((1+1*2)+(3*4*5))/3", expr(_), instrument = instrument)

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
        parse("((1+1*2)+(3*4*))/3", expr(_), instrument = instrument)

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
