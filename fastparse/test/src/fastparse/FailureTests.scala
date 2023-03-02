package test.fastparse
import fastparse._

import utest._

object FailureTests extends TestSuite{
  def tests = Tests{

    def checkOffset(input: String,
                    expected: String,
                    label: String,
                    terminals: String = null,
                    parser: P[_] => P[_]) = {
      val f @ Parsed.Failure(failureString, index, extra) = parse(input, parser(_))

      val trace = f.trace(true)

      val terminals1 = Option(terminals).getOrElse(expected)
      assert(
        trace.failure.label == label,
        trace.groupAggregateString == expected,
        trace.terminalAggregateString == terminals1
      )
    }

    test("simple"){
      import NoWhitespace._
      def check(parser: P[_] => P[_]) = {
        val f @ Parsed.Failure(failureString, index, extra) = parse("d", parser(_))
        val trace = f.trace(true)

        assert(
          trace.terminalAggregateString == """("a" | "b" | "c")""",
          trace.groupAggregateString == """(parseB | "c")"""
        )
      }

      test("either") - check{
        def parseB[$: P] = P( "a" | "b" )
        def parseA[$: P] = P( (parseB | "") ~ "c" )
        parseA(_)
      }
      test("option") - check{
        def parseB[$: P] = P( "a" | "b" )
        def parseA[$: P] = P( parseB.? ~ "c" )
        parseA(_)
      }
      test("rep") - check{
        def parseB[$: P] = P( "a" | "b" )
        def parseA[$: P] = P( parseB.rep ~ "c" )
        parseA(_)
      }
      test("repApply") - check{
        def parseB[$: P] = P( "a" | "b" )
        def parseA[$: P] = P( parseB.rep() ~ "c" )
        parseA(_)
      }
      test("repX") - check{
        def parseB[$: P] = P( "a" | "b" )
        def parseA[$: P] = P( parseB.repX ~ "c" )
        parseA(_)
      }
      test("repXApply") - check{
        def parseB[$: P] = P( "a" | "b" )
        def parseA[$: P] = P( parseB.repX() ~ "c" )
        parseA(_)
      }
      test("deep"){
        test("option") - check{
          def parseC[$: P] = P( "a" | "b" )
          def parseB[$: P] = P( parseC.rep(1) )
          def parseA[$: P] = P( parseB.? ~ "c" )
          parseA(_)
        }
        test("either") - check{
          def parseC[$: P] = P( "a" | "b" )
          def parseB[$: P] = P( parseC.rep(1) )
          def parseA[$: P] = P( (parseB | "") ~ "c" )
          parseA(_)
        }
        test("rep") - check{
          def parseC[$: P] = P( "a" | "b" )
          def parseB[$: P] = P( parseC.rep(1) )
          def parseA[$: P] = P( parseB.rep ~ "c" )
          parseA(_)
        }
        test("repApply") - check{
          def parseC[$: P] = P( "a" | "b" )
          def parseB[$: P] = P( parseC.rep(1) )
          def parseA[$: P] = P( parseB.rep() ~ "c" )
          parseA(_)
        }
        test("repX") - check{
          def parseC[$: P] = P( "a" | "b" )
          def parseB[$: P] = P( parseC.repX(1) )
          def parseA[$: P] = P( parseB.repX ~ "c" )
          parseA(_)
        }
        test("repXApply") - check{
          def parseC[$: P] = P( "a" | "b" )
          def parseB[$: P] = P( parseC.repX(1) )
          def parseA[$: P] = P( parseB.repX() ~ "c" )
          parseA(_)
        }
      }
    }

    test("misc"){
      import NoWhitespace._
      test("sep"){
        def parseB[$: P] = P( "a" | "b" )
        def parseA[$: P] = P( parseB.rep(sep = ",") ~ "c" )
        val f1 @ Parsed.Failure(_, _, _) = parse("ad", parseA(_))

        val trace = f1.trace()

        assert(trace.groupAggregateString == """("," | "c")""")

        val f2 @ Parsed.Failure(_, _, _) = parse("a,d", parseA(_))
        val trace2 = f2.trace()

        // Make sure if the parse fails after the separator and has to backtrack,
        // we list both the separator and the post-separator parse that failed
        // since showing the separator alone (which passed) is misleading
        println(f2.index)
        assert(trace2.groupAggregateString == """("," ~ parseB | "c")""")
        f2.index
      }

      test("sepCut"){
        def parseB[$: P] = P( "a" | "b" | "c" )
        def parseA[$: P] = P( parseB.rep(sep = ","./) ~ "d" )
        val f1 @ Parsed.Failure(_, _, _) = parse("ax", parseA(_))
        val trace = f1.trace()

        trace.groupAggregateString ==> """("," | "d")"""
        f1.index ==> 1

        val f2 @ Parsed.Failure(_, _, _) = parse("a,x", parseA(_))
        val trace2 = f2.trace()

        trace2.groupAggregateString ==> """("a" | "b" | "c")"""
        f2.index ==> 2
      }

      test("aggregateInNamedParser") - checkOffset(
        input = "ad",
        expected = """("b" | "c")""",
        label = "\"c\"",
        parser = {
          def parseB[$: P] = P("a" ~ "b".?)
          def parseA[$: P] = P(parseB ~ "c")
          parseA(_)
        }
      )

      test("manualSep") - checkOffset(
        input = "a,d",
        expected = "\"c\"",
        label = "\"c\"",
        parser = {
          def parseA[$: P] = P( "a" ~ (("." | ","./) ~ "c").rep ~ "x" )
          parseA(_)
        }
      )

      test("sequentialEithers") - checkOffset(
        input = "z",
        expected = """("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "x")""",
        label = "\"x\"",
        parser = {
          def parseD[$: P] = P( (("m" | "n") | "o").rep )
          def parseC[$: P] = P( (("g" | "h") | "i").? )
          def parseB[$: P] = P( ("a" | "b") | "c" | "" )
          def parseA[$: P] = P(
            parseB ~ ("d" | ("e" | "f") | "") ~
              parseC ~ ("j" | ("k" | "l")).? ~
              parseD ~ ("p" | ("q" | "r")).rep ~
              "x"
          )
          parseA(_)
        }
      )
      test("passingNamedParsersAggregateIsShallow") - checkOffset(
        input = "ad",
        expected = """(parseB ~ fail | "c")""",
        label = "\"c\"",
        terminals = "\"c\"",
        parser = {
          def parseB[$: P] = P( "a" ~ "b".? )
          def parseA[$: P] = P( (parseB ~ Fail).? ~ "c" )
          parseA(_)
        }
      )

      test("passingNamedParsersEitherAggregateIsShallow") - checkOffset(
        input = "_",
        expected = """(parseB | parseZ)""",
        label = "(parseB | parseZ)",
        terminals = """("c" | "d" | "x" | "y")""",
        parser = {
          def parseD[$: P] = P("d")
          def parseC[$: P] = P("c")
          def parseX[$: P] = P("x")
          def parseY[$: P] = P("y")
          def parseB[$: P] = P(parseC | parseD)
          def parseZ[$: P] = P(parseX | parseY)
          def parseA[$: P] = P(parseB | parseZ)
          parseA(_)
        }
      )
    }

    test("offset"){
      import NoWhitespace._
      // Consider cases where the failure happens down some branch of an either,
      // rep, or option, where the branch takes place before traceIndex but the
      // actual failure lines up nicely.
      test("opt") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").? ~ "a" ~ "d" }
      )
      test("optRep") - checkOffset(
        input = "ax",
        expected = """("b".rep(1) | "d")""",
        label = "\"d\"",
        terminals = """("b" | "d")""",
        parser = { implicit c => ("a" ~ "b".rep(1)).? ~ "a" ~ "d" }
      )
      test("opt3") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => (("a" ~ "b".?) ~ "c").? ~ "a" ~ "d"}
      )

      test("opt4") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d" | "e" | "f")""",
        label = "\"f\"",
        parser = { implicit c => (("a" ~ "b".? ~ "c".?) ~ "d".? ~ "e").? ~ "a" ~ "f"}
      )

      test("opt5") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ ("b".? ~ "c")).? ~ "a" ~ "d" }
      )

      test("rep") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").rep ~ "a" ~ "d" }
      )
      test("rep_3") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => (("a" ~ "b".rep) ~ "c").rep ~ "a" ~ "d"}
      )

      test("rep_4") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ ("b".rep ~ "c")).rep ~ "a" ~ "d" }
      )

      test("repX") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").repX ~ "a" ~ "d" }
      )
      test("repSep") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").rep(sep = Pass) ~ "a" ~ "d" }
      )
      test("repXSep") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").repX(sep = Pass) ~ "a" ~ "d" }
      )
      test("rep1") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").rep(1).? ~ "a" ~ "d" }
      )
      test("repX1") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").repX(1).? ~ "a" ~ "d" }
      )
      test("rep1Sep") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c =>
          ("a" ~ "b").rep(1, sep = Pass).? ~ "a" ~ "d"
        }
      )
      test("repX1Sep") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").repX(1, sep = Pass).? ~ "a" ~ "d" }
      )
      test("either1") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => "a" ~ "b" | "a" ~/ "d" }
      )
      test("either2") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d" | "e")""",
        label = "\"e\"",
        parser = {implicit c => (("a" ~ "b" | "a" ~ "c") | "a" ~ "d" | "") ~ "a" ~ "e" }
      )
      test("either3") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d" | "e")""",
        label = "\"e\"",
        parser = {implicit c => ("a" ~ "b" | ("a" ~ "c" | "a" ~ "d") | "") ~ "a" ~ "e" }
      )

      test("opEither") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b" | "a" ~ "c").? ~ "a" ~ "d" }
      )
      test("optEitherRep1") - checkOffset(
        input = "ax",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => (("a" ~ "b").rep(1, sep = Pass) | ("a" ~ "c")).? ~ "a" ~ "d" }
      )

      test("optOpt") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").?.? ~ "a" ~ "d" }
      )
      test("optRepX") - checkOffset(
        input = "ax",
        expected = """("b" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ "b").repX.? ~ "a" ~ "d" }
      )
    }

    test("downstream"){
      import NoWhitespace._
      // In the case where one branch fails further in than `traceIndex`, we
      // collect the partial aggregation from that branch in the
      // `failureGroupAggregate` but ignore that branch's downstream failure in
      // `failureTerminalsAggregate`

      def check(parser: P[_] => P[_]) = checkOffset(
        input = "abx",
        expected = """("b" ~ "c" | "d")""",
        label = "\"d\"",
        terminals = "\"d\"",
        parser = parser
      )

      test("opt") -        check{ implicit c => ("a" ~ ("b" ~ "c")).? ~ "a" ~/ "d" }
      test("optLeft") -    check{ implicit c => (("a" ~ "b") ~ "c").? ~ "a" ~ "d" }
      test("opt2") -        check{ implicit c => ("a".! ~ ("b".! ~ "c".!)).? ~ "a".! ~/ "d".! }
      test("optLeft2") -    check{ implicit c => (("a".! ~ "b".!) ~ "c".!).? ~ "a".! ~ "d".! }

      test("either1") -    check{ implicit c => (("a" ~ "b") ~ "c") | "a" ~/ "d" }
      test("either2") -    check{ implicit c => "a" ~ ("b" ~ "c") | "a" ~/ "d" }

      test("either3") -    check{ implicit c => ("a" ~ ("b" ~ "c") | "") ~ "a" ~/ "d" }

      test("rep") -        check{ implicit c => ("a" ~ ("b" ~ "c")).rep ~ "a" ~/ "d" }

      test("repApply") -   check{ implicit c => ("a" ~ ("b" ~ "c")).rep() ~ "a" ~/ "d" }
      test("repLeft") -    check{ implicit c => (("a" ~ "b") ~ "c").rep ~ "a" ~/ "d" }
      test("repX") -       check{ implicit c => ("a" ~ ("b" ~ "c")).repX ~ "a" ~/ "d" }
      test("repXLeft") -   check{ implicit c => (("a" ~ "b") ~ "c").repX ~ "a" ~/ "d" }
      test("repSep") -     check{ implicit c => ("a" ~ ("b" ~ "c")).rep(sep = Pass) ~ "a" ~/ "d" }
      test("repSepLeft") - check{ implicit c => (("a" ~ "b") ~ "c").rep(sep = Pass) ~ "a" ~/ "d" }
    }

    test("whitespace"){
      import SingleLineWhitespace._
      test("opt3") - checkOffset(
        input = "a x",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => (("a" ~ "b".?) ~ "c").? ~ "a" ~ "d"}
      )

      test("opt4") - checkOffset(
        input = "a  x",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => ("a" ~ ("b".? ~ "c")).? ~ "a" ~ "d" }
      )

      test("optEitherRep1") - checkOffset(
        input = "a   x",
        expected = """("b" | "c" | "d")""",
        label = "\"d\"",
        parser = { implicit c => (("a" ~ "b").rep(1, sep = Pass) | ("a" ~ "c")).? ~ "a" ~ "d" }
      )

      test("opt2") - checkOffset(
        input = "a  b   x",
        expected = """("b" ~ "c" | "d")""",
        label = "\"d\"",
        terminals = "\"d\"",
        parser = {implicit c => ("a".! ~ ("b".! ~ "c".!)).? ~ "a".! ~/ "d".!}
      )

      test("optLeft2") - checkOffset(
        input = "a   b  x",
        expected = """("b" ~ "c" | "d")""",
        label = "\"d\"",
        terminals = "\"d\"",
        parser = { implicit c => (("a".! ~ "b".!) ~ "c".!).? ~ "a".! ~ "d".! }
      )
    }
  }
}
