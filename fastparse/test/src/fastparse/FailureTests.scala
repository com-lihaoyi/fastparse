package test.fastparse
import fastparse._, NoWhitespace._
import utest._
object FailureTests extends TestSuite{
  def tests = Tests{
    def checkSimple(parser: P[_] => P[_]) = {
      val f @ Parsed.Failure(failureString, index, extra) = parse("d", parser(_))
      val trace = f.trace()

      assert(trace.groupAggregateString == """(parseB | "c")""")
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
      val f @ Parsed.Failure(failureString, index, extra) = parse("ad", parseA(_))
      val trace = f.trace()

      assert(trace.groupAggregateString == """("," | "c")""")
    }
  }
}
