package test.fastparse
import fastparse._
import utest._

import scala.collection.mutable

/**
  * Demonstrates simultaneously parsing and
  * evaluating simple arithmetic expressions
  */
object MathTests extends TestSuite{
  import fastparse.NoWhitespace._
  def eval(tree: (Int, Seq[(String, Int)])) = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right case "-" => left - right
      case "*" => left * right case "/" => left / right
    }}
  }

  def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )
  def parens[_: P]: P[Int] = P( "(" ~/ addSub ~ ")" )
  def factor[_: P]: P[Int] = P( number | parens )

  def divMul[_: P]: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  def addSub[_: P]: P[Int] = P( divMul ~ (CharIn("+\\-").! ~/ divMul).rep ).map(eval)
  def expr[_: P]: P[Int]   = P( addSub ~ End )

  val tests = Tests {
    'pass - {
      val Parsed.Success(2, _) = parse("1+1").read(expr(_))
      val Parsed.Success(15, _) = parse("(1+1*2)+3*4").read(expr(_))
      val Parsed.Success(21, _) = parse("((1+1*2)+(3*4*5))/3").read(expr(_))
      val Parsed.Failure(failIndex, expected, extra) = parse("1+1*").read(expr(_))
      assert(
        failIndex == 4,
        extra.traced.trace == """Expected expr:1:1 / addSub:1:1 / divMul:1:3 / factor:1:5 / ([0-9] | "("):1:5, found """"")
    }
    'fail - {
      def check(input: String, expectedTrace: String, expectedShortTrace: String) = {
        val failure = parse(input).read(expr(_)).asInstanceOf[Parsed.Failure]
        val actualTrace = failure.extra.traced.trace
        assert(expectedTrace.trim == actualTrace.trim)

        // Check iterator parsing results in a failure in the right place. Note
        // that we aren't checking the `.traced.trace` because that requires a
        // second parse which doesn't work with iterators (which get exhausted)
        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)) {
          val failure2 = parseIter(input.grouped(chunkSize)).read(expr(_)).asInstanceOf[Parsed.Failure]
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
        """Expected expr:1:1 / addSub:1:1 / divMul:1:1 / factor:1:1 / parens:1:1 / addSub:1:2 / divMul:1:4 / factor:1:4 / parens:1:4 / ([0-9] | [*/] | [+\-] | ")"):1:8, found "x))+4"""",
        """Expected ")":7, found "x))+4""""
      )
    }
  }
}
