package test.fastparse
import fastparse._, NoWhitespace._
import utest._
object FailureTests extends TestSuite{
  def tests = Tests{
    def checkSimple(parser: P[_] => P[_]) = {
      val f @ Parsed.Failure(failureString, index, extra) = parse("d", parser(_))
      val trace = f.trace()

      assert(
        trace.terminalAggregateString == """("a" | "b" | "c")""",
        trace.groupAggregateString == """(parseB | "c")"""
      )
    }

    'simple - {
      'either - checkSimple{
        def parseB[_: P] = P( "a" | "b" )
        def parseA[_: P] = P( (parseB | "") ~ "c" )
        parseA(_)
      }
      'option - checkSimple{
        def parseB[_: P] = P( "a" | "b" )
        def parseA[_: P] = P( parseB.? ~ "c" )
        parseA(_)
      }
      'rep - checkSimple{
        def parseB[_: P] = P( "a" | "b" )
        def parseA[_: P] = P( parseB.rep ~ "c" )
        parseA(_)
      }
      'repApply - checkSimple{
        def parseB[_: P] = P( "a" | "b" )
        def parseA[_: P] = P( parseB.rep() ~ "c" )
        parseA(_)
      }
      'repX - checkSimple{
        def parseB[_: P] = P( "a" | "b" )
        def parseA[_: P] = P( parseB.repX ~ "c" )
        parseA(_)
      }
      'repXApply - checkSimple{
        def parseB[_: P] = P( "a" | "b" )
        def parseA[_: P] = P( parseB.repX() ~ "c" )
        parseA(_)
      }
    }

    'deep - {
      'option - checkSimple{
        def parseC[_: P] = P( "a" | "b" )
        def parseB[_: P] = P( parseC.rep(1) )
        def parseA[_: P] = P( parseB.? ~ "c" )
        parseA(_)
      }
      'either - checkSimple{
        def parseC[_: P] = P( "a" | "b" )
        def parseB[_: P] = P( parseC.rep(1) )
        def parseA[_: P] = P( (parseB | "") ~ "c" )
        parseA(_)
      }
      'rep - checkSimple{
        def parseC[_: P] = P( "a" | "b" )
        def parseB[_: P] = P( parseC.rep(1) )
        def parseA[_: P] = P( parseB.rep ~ "c" )
        parseA(_)
      }
      'repApply - checkSimple{
        def parseC[_: P] = P( "a" | "b" )
        def parseB[_: P] = P( parseC.rep(1) )
        def parseA[_: P] = P( parseB.rep() ~ "c" )
        parseA(_)
      }
      'repX - checkSimple{
        def parseC[_: P] = P( "a" | "b" )
        def parseB[_: P] = P( parseC.repX(1) )
        def parseA[_: P] = P( parseB.repX ~ "c" )
        parseA(_)
      }
      'repXApply - checkSimple{
        def parseC[_: P] = P( "a" | "b" )
        def parseB[_: P] = P( parseC.repX(1) )
        def parseA[_: P] = P( parseB.repX() ~ "c" )
        parseA(_)
      }
    }

    'sep - {
      def parseB[_: P] = P( "a" | "b" )
      def parseA[_: P] = P( parseB.rep(sep = ",") ~ "c" )
      val f1 @ Parsed.Failure(_, _, _) = parse("ad", parseA(_))
      val trace = f1.trace()

      assert(trace.groupAggregateString == """("," | "c")""")

      val f2 @ Parsed.Failure(_, _, _) = parse("a,d", parseA(_))
      val trace2 = f2.trace()

      // Make sure if the parse fails after the separator and has to backtrack,
      // we list both the separator and the post-separator parse that failed
      // since showing the separator alone (which passed) is misleading
      assert(trace2.groupAggregateString == """("," ~ parseB | "c")""")
      f2.index
    }

    'sepCut - {
      def parseB[_: P] = P( "a" | "b" )
      def parseA[_: P] = P( parseB.rep(sep = ","./) ~ "c" )
      val f1 @ Parsed.Failure(_, _, _) = parse("ad", parseA(_))
      val trace = f1.trace()

      assert(trace.groupAggregateString == """("," | "c")""")

      val f2 @ Parsed.Failure(_, _, _) = parse("a,d", parseA(_))
      val trace2 = f2.trace()

      assert(trace2.groupAggregateString == """("a" | "b")""")
      f2.index
    }

    'aggregateInNamedParser - {
      def parseB[_: P] = P( "a" ~ "b".? )
      def parseA[_: P] = P( parseB ~ "c" )
      val f1 @ Parsed.Failure(_, _, _) = parse("ad", parseA(_))
      val trace = f1.trace()

      assert(trace.groupAggregateString == """("b" | "c")""")
    }

    'passingNamedParsersAggregateIsShallow - {
      def parseB[_: P] = P( "a" ~ "b".? )
      def parseA[_: P] = P( (parseB ~ Fail).? ~ "c" )
      val f1 @ Parsed.Failure(_, _, _) = parse("ad", parseA(_))
      val trace = f1.trace()
      assert(trace.groupAggregateString == """(parseB ~ fail | "c")""")
    }
  }
}
