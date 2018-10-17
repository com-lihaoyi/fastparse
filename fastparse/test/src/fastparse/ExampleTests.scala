package test.fastparse

import utest._
import fastparse._
import fastparse.internal.Logger
/**
  * Demonstrates simultaneously parsing and
  * evaluating simple arithmetic expressions
  */
object ExampleTests extends TestSuite{
  import fastparse.NoWhitespace._
  val tests = Tests{
    'basic{
      'simple {
        import fastparse._, NoWhitespace._
        def parseA[_: P] = P( "a" )

        val Parsed.Success(value, successIndex) = parse("a", parseA(_))
        assert(value == (), successIndex == 1)

        val failure = parse("b", parseA(_)).asInstanceOf[Parsed.Failure]
        val trace = failure.traced.trace
        assert(
//          failure.stack == List(("\"a\"", 0)),
//          failure.toString == """Parsed.Failure(Expected "a":1:1, found "b")""",
//          trace == """Expected parseA:1:1 / "a":1:1, found "b""""
        )
        trace
      }

      'sequence {
        def ab[_: P] = P( "a" ~ "b" )

        val Parsed.Success(_, 2) = parse("ab", ab(_))

        val Parsed.Failure(_, 1, _) = parse("aa", ab(_))
      }
      'repeat{
        def ab[_: P] = P( "a".rep ~ "b" )
        val Parsed.Success(_, 8) = parse("aaaaaaab", ab(_))
        val Parsed.Success(_, 4) = parse("aaaba", ab(_))

        def abc[_: P] = P( "a".rep(sep="b") ~ "c")
        val Parsed.Success(_, 8) = parse("abababac", abc(_))
        val Parsed.Failure(_, 3, _) = parse("abaabac", abc(_))

        def ab4[_: P] = P( "a".rep(min=2, max=4, sep="b") )
        val Parsed.Success(_, 7) = parse("ababababababa", ab4(_))

        def ab2exactly[_: P] = P( "ab".rep(exactly=2) )
        val Parsed.Success(_, 4) = parse("abab", ab2exactly(_))

        def ab4c[_: P] = P ( "a".rep(min=2, max=4, sep="b") ~ "c" )
        val Parsed.Failure(_, 1, _) = parse("ac", ab4c(_))
        val Parsed.Success(_, 4) = parse("abac", ab4c(_))
        val Parsed.Success(_, 8) = parse("abababac", ab4c(_))
        val Parsed.Failure(_, 7, _) = parse("ababababac", ab4c(_))
      }

      'option{
        def option[_: P] = P( "c".? ~ "a".rep(sep="b").! ~ End)

        val Parsed.Success("aba", 3) = parse("aba", option(_))
        val Parsed.Success("aba", 3) = parse("aba", option(_))
      }

      'either{
        def either[_: P] = P( "a".rep ~ ("b" | "c" | "d") ~ End)

        val Parsed.Success(_, 6) = parse("aaaaab", either(_))
        val f @ Parsed.Failure(_, 5, _) = parse("aaaaae", either(_))
        val trace = f.traced.trace
        assert(
          f.toString == """Parsed.Failure(Position 1:6, found "e")""",
          trace == """Expected either:1:1 / ("a" | "b" | "c" | "d"):1:6, found "e""""
        )
      }


      'end{
        def noEnd[_: P] = P( "a".rep ~ "b")
        def withEnd[_: P] = P( "a".rep ~ "b" ~ End)

        val Parsed.Success(_, 4) = parse("aaaba", noEnd(_))
        val Parsed.Failure(_, 4, _) = parse("aaaba", withEnd(_))

      }
      'start{
        def ab[_: P] = P( (("a" | Start) ~ "b").rep ~ End).!

        val Parsed.Success("abab", 4) = parse("abab", ab(_))
        val Parsed.Success("babab", 5) = parse("babab", ab(_))

        val Parsed.Failure(_, 2, _) = parse("abb", ab(_))

      }

      'passfail{
        val Parsed.Success((), 0) = parse("asdad", Pass(_))
        val Parsed.Failure(_, 0, _) = parse("asdad", Fail(_))
      }

      'index{
        def finder[_: P] = P( "hay".rep ~ Index ~ "needle" ~ "hay".rep )

        val Parsed.Success(9, _) = parse("hayhayhayneedlehay", finder(_))
      }

      'capturing{
        def capture1[_: P] = P( "a".rep.! ~ "b" ~ End)

        val Parsed.Success("aaa", 4) = parse("aaab", capture1(_))

        def capture2[_: P] = P( "a".rep.! ~ "b".! ~ End)

        val Parsed.Success(("aaa", "b"), 4) = parse("aaab", capture2(_))

        def capture3[_: P] = P( "a".rep.! ~ "b".! ~ "c".! ~ End)

        val Parsed.Success(("aaa", "b", "c"), 5) = parse("aaabc", capture3(_))

        def captureRep[_: P] = P( "a".!.rep ~ "b" ~ End)

        val Parsed.Success(Seq("a", "a", "a"), 4) = parse("aaab", captureRep(_))

        def captureOpt[_: P] = P( "a".rep ~ "b".!.? ~ End)

        val Parsed.Success(Some("b"), 4) = parse("aaab", captureOpt(_))
      }

      'anychar{
        def ab[_: P] = P( "'" ~ AnyChar.! ~ "'" )

        val Parsed.Success("-", 3) = parse("'-'", ab(_))

        val Parsed.Failure(stack, 2, _) = parse("'-='", ab(_))
      }


      'lookahead{
        def keyword[_: P] = P( ("hello" ~ &(" ")).!.rep )

        val Parsed.Success(Seq("hello"), _) = parse("hello ", keyword(_))
        val Parsed.Success(Seq(), __) = parse("helloX", keyword(_))
      }
      'neglookahead{
        def keyword[_: P] = P( "hello" ~ !" " ~ AnyChar ~ "world" ).!

        val Parsed.Success("hello-world", _) = parse("hello-world", keyword(_))
        val Parsed.Success("hello_world", _) = parse("hello_world", keyword(_))

        val Parsed.Failure(_, 5, _) = parse("hello world", keyword(_))
//        assert(parser == !(" "))
      }
      'map{
        def binary[_: P] = P( ("0" | "1" ).rep.! )
        def binaryNum[_: P] = P( binary.map(Integer.parseInt(_, 2)) )

        val Parsed.Success("1100", _) = parse("1100", binary(_))
        val Parsed.Success(12, _) = parse("1100", binaryNum(_))
      }
      'flatMap{
        def leftTag[_: P] = P( "<" ~ (!">" ~ AnyChar).rep(1).! ~ ">")
        def rightTag[_: P](s: String) = P( "</" ~ s.! ~ ">" )
        def xml[_: P] = P( leftTag.flatMap(rightTag) )

        val Parsed.Success("a", _) = parse("<a></a>", xml(_))
        val Parsed.Success("abcde", _) = parse("<abcde></abcde>", xml(_))

        val failure = parse("<abcde></edcba>", xml(_)).asInstanceOf[Parsed.Failure]
        assert(
          failure.traced.trace == """Expected xml:1:1 / rightTag:1:8 / "abcde":1:10, found "edcba>""""
        )
      }
      'flatMapFor{
        def leftTag[_: P] = P( "<" ~ (!">" ~ AnyChar).rep(1).! ~ ">" )
        def rightTag[_: P](s: String) = P( "</" ~ s.! ~ ">" )
        def xml[_: P] = P(
          for{
            s <- leftTag
            right <- rightTag(s)
          } yield right
        )

        val Parsed.Success("a", _) = parse("<a></a>", xml(_))
        val Parsed.Success("abcde", _) = parse("<abcde></abcde>", xml(_))

        val failure = parse("<abcde></edcba>", xml(_)).asInstanceOf[Parsed.Failure]
        assert(
          failure.extra.traced.trace == """Expected xml:1:1 / rightTag:1:8 / "abcde":1:10, found "edcba>""""
        )
      }
      'filter{
        def digits[_: P] = P(CharPred(c => '0' <= c && c <= '9').rep(1).!).map(_.toInt)
        def even[_: P] = P( digits.filter(_ % 2 == 0) )
        val Parsed.Success(12, _) = parse("12", even(_))
        val failure = parse("123", even(_)).asInstanceOf[Parsed.Failure]
      }
      'opaque{
        def digit[_: P] = CharIn("0-9")
        def letter[_: P] = CharIn("A-Z")
        def twice[T, _: P](p: => P[T]) = p ~ p
        def errorMessage[T](p: P[_] => P[T], str: String) =
          parse(str, p).asInstanceOf[Parsed.Failure].traced.trace

        // Portuguese number plate format since 2006
        def numberPlate[_: P] = P(twice(digit) ~ "-" ~ twice(letter) ~ "-" ~ twice(digit))

        val err1 = errorMessage(numberPlate(_), "11-A1-22")
        assert(err1 == """Expected numberPlate:1:1 / [A-Z]:1:5, found "1-22"""")

        // Suppress implementation details from the error message
        def opaqueNumberPlate[_: P] = numberPlate.opaque("<number-plate>")

        val err2 = errorMessage(opaqueNumberPlate(_), "11-A1-22")
        assert(err2 == """Expected <number-plate>:1:1, found "11-A1-22"""")
      }
    }
    'charX{
      'charPred{
        def cp[_: P] = P( CharPred(_.isUpper).rep.! ~ "." ~ End )

        val Parsed.Success("ABC", _) = parse("ABC.", cp(_))
        val Parsed.Failure(_, 2, _) = parse("ABc.", cp(_))
      }

      'charIn{
        def ci[_: P] = P( CharIn("abc", "xyz").rep.! ~ End )

        val Parsed.Success("aaabbccxyz", _) = parse("aaabbccxyz", ci(_))
        val Parsed.Failure(_, 7, _) = parse("aaabbccdxyz.", ci(_))


        def digits[_: P] = P( CharIn("0-9").rep.! )

        val Parsed.Success("12345", _) = parse("12345abcde", digits(_))
        val Parsed.Success("123", _) = parse("123abcde45", digits(_))
      }

      'charsWhile{
        def cw[_: P] = P( CharsWhile(_ != ' ').! )

        val Parsed.Success("12345", _) = parse("12345", cw(_))
        val Parsed.Success("123", _) = parse("123 45", cw(_))
      }
      'charsWhileIn{
        def cw[_: P] = P( CharsWhileIn("123456789").! )

        val Parsed.Success("12345", _) = parse("12345", cw(_))
        val Parsed.Success("123", _) = parse("123 45", cw(_))
      }

      'stringIn{
        def si[_: P] = P( StringIn("cow", "cattle").!.rep(1) )

        val Parsed.Success(Seq("cow", "cattle"), _) = parse("cowcattle", si(_))
        val Parsed.Success(Seq("cow"), _) = parse("cowmoo", si(_))
        val Parsed.Failure(_, _, _) = parse("co", si(_))
      }
    }

    'cuts{
      'nocut{
        def alpha[_: P] = P( CharIn("a-z") )
        def nocut[_: P] = P( "val " ~ alpha.rep(1).! | "def " ~ alpha.rep(1).!)

        val Parsed.Success("abcd", _) = parse("val abcd", nocut(_))

        val failure = parse("val 1234", nocut(_)).asInstanceOf[Parsed.Failure]
        val trace = failure.extra.traced.trace
        assert(
          failure.index == 0,
          trace == """Expected nocut:1:1 / ("val " ~ alpha.rep(1).! | "def "):1:1, found "val 1234""""
        )
      }

      'withcut{
        def alpha[_: P] = P( CharIn("a-z") )
        def nocut[_: P] = P( "val " ~/ alpha.rep(1).! | "def " ~/ alpha.rep(1).!)

        val Parsed.Success("abcd", _) = parse("val abcd", nocut(_))

        val failure = parse("val 1234", nocut(_)).asInstanceOf[Parsed.Failure]
        val trace = failure.extra.traced.trace
        assert(
          failure.index == 4,
          trace == """Expected nocut:1:1 / alpha:1:5 / [a-z]:1:5, found "1234""""
        )
      }

      'repnocut{
        def alpha[_: P] = P( CharIn("a-z") )
        def stmt[_: P] = P( "val " ~ alpha.rep(1).! ~ ";" ~ " ".rep )
        def stmts[_: P] = P( stmt.rep(1) ~ End )

        val Parsed.Success(Seq("abcd"), _) = parse("val abcd;", stmts(_))
        val Parsed.Success(Seq("abcd", "efg"), _) = parse("val abcd; val efg;", stmts(_))

        val failure = parse("val abcd; val ", stmts(_)).asInstanceOf[Parsed.Failure]
        val trace = failure.extra.traced.trace
        assert(
          failure.index == 10,
          trace == """Expected stmts:1:1 / (" " | stmt.rep(1) | end-of-input):1:11, found "val """"
        )
      }

      'repcut{
        def alpha[_: P] = P( CharIn("a-z") )
        def stmt[_: P] = P( "val " ~/ alpha.rep(1).! ~ ";" ~ " ".rep )
        def stmts[_: P] = P( stmt.rep(1) ~ End )

        val Parsed.Success(Seq("abcd"), _) = parse("val abcd;", stmts(_))
        val Parsed.Success(Seq("abcd", "efg"), _) = parse("val abcd; val efg;", stmts(_))

        val failure = parse("val abcd; val ", stmts(_)).asInstanceOf[Parsed.Failure]
        val trace = failure.extra.traced.trace
        assert(
          failure.index == 14,
          trace == """Expected stmts:1:1 / stmt:1:11 / alpha:1:15 / [a-z]:1:15, found """""
        )
      }
      'delimiternocut{
        def digits[_: P] = P( CharIn("0-9").rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=",") ~ ")" )

        val Parsed.Success(Seq("1", "23"), _) = parse("(1,23)", tuple(_))

        val failure = parse("(1,)", tuple(_)).asInstanceOf[Parsed.Failure]
        val trace = failure.extra.traced.trace
        assert(
          failure.index == 2,
          trace == """Expected tuple:1:1 / ([0-9] | ")"):1:3, found ",)""""
        )
      }
      'delimitercut{
        def digits[_: P] = P( CharIn("0-9").rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=","./) ~ ")" )

        val Parsed.Success(Seq("1", "23"), _) = parse("(1,23)", tuple(_))

        val failure = parse("(1,)", tuple(_)).asInstanceOf[Parsed.Failure]
        val index = failure.index
        val trace = failure.extra.traced.trace
        assert(
          index == 3,
          trace == """Expected tuple:1:1 / digits:1:4 / [0-9]:1:4, found ")""""
        )
      }
      'endcut{
        def digits[_: P] = P( CharIn("0-9").rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=","./) ~ ")" )

        val Parsed.Success(Seq("1", "23"), _) = parse("(1,23)", tuple(_))

        val failure = parse("(1,)", tuple(_)).asInstanceOf[Parsed.Failure]
        val trace = failure.extra.traced.trace
        assert(
          failure.index == 3,
          trace == """Expected tuple:1:1 / digits:1:4 / [0-9]:1:4, found ")""""
        )

      }
      'composecut{
        def digit[_: P] = P( CharIn("0-9") )
        def time1[_: P] = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
        def time2[_: P] = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
        val Parsed.Success((), _) = parse("12:30pm", time1(_))
        val Parsed.Success((), _) = parse("17:45", time2(_))
        def time[_: P] = P( time1 | time2 ).log
        val Parsed.Success((), _) = parse("12:30pm", time(_))
        val failure = parse("17:45", time(_)).asInstanceOf[Parsed.Failure]
        assert(failure.index == 5)  // Expects am or pm
      }
      'composenocut{
        def digit[_: P] = P( CharIn("0-9") )
        def time1[_: P] = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
        def time2[_: P] = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
        val Parsed.Success((), _) = parse("12:30pm", time1(_))
        val Parsed.Success((), _) = parse("17:45", time2(_))
        def time[_: P] = P( NoCut(time1) | time2 )
        val Parsed.Success((), _) = parse("12:30pm", time(_))
        val Parsed.Success((), _) = parse("17:45", time(_))
      }
    }
    'debugging{
      def check(a: Any, s: String) = assert(a.toString == s.trim)
      'original{
        object Foo{

          def plus[_: P] = P( "+" )
          def num[_: P] = P( CharIn("0-9").rep(1) ).!.map(_.toInt)
          def side[_: P] = P( "(" ~ expr ~ ")" | num )
          def expr[_: P]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}
        }


        check(
          parse("(1+(2+3x))+4", Foo.expr(_)),
          """Parsed.Failure(Position 1:1, found "(1+(2+3x))")"""
        )
      }
      'cuts{
        object Foo{

          def plus[_: P] = P( "+" )
          def num[_: P] = P( CharIn("0-9").rep(1) ).!.map(_.toInt)
          def side[_: P] = P( "(" ~/ expr ~ ")" | num )
          def expr[_: P]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}
        }
        check(
          parse("(1+(2+3x))+4", Foo.expr(_)),
          """Parsed.Failure(Position 1:8, found "x))+4")"""
        )
      }
      'logSimple - {
        val logged = collection.mutable.Buffer.empty[String]
        implicit val logger = Logger(logged.append(_))

        def DeepFailure[_: P] = P( "C" ).log
        def Foo[_: P] = P( (DeepFailure | "A".log("A", logger)) ~ "B".!.log("B", logger) ).log

        parse("AB", Foo(_))

        val allLogged = logged.mkString("\n")

        val expected =
          """+Foo:1:1, cut
            |  +DeepFailure:1:1
            |  -DeepFailure:1:1:Failure(DeepFailure:1:1 / "C":1:1 ..."AB")
            |  +A:1:1
            |  -A:1:1:Success(1:2)
            |  +B:1:2, cut
            |  -B:1:2:Success(1:3, cut)
            |-Foo:1:1:Success(1:3, cut)
            |
        """.stripMargin.trim
        assert(allLogged == expected)
      }
      'log{
        val captured = collection.mutable.Buffer.empty[String]
        implicit val logger = Logger(captured.append(_))
        object Foo{

          def plus[_: P] = P( "+" )
          def num[_: P] = P( CharIn("0-9").rep(1) ).!.map(_.toInt)
          def side[_: P] = P( "(" ~/ expr ~ ")" | num ).log
          def expr[_: P]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}.log
        }


        parse("(1+(2+3x))+4", Foo.expr(_))


        val expected = """
          +expr:1:1, cut
            +side:1:1, cut
              +expr:1:2, cut
                +side:1:2, cut
                -side:1:2:Success(1:3, cut)
                +side:1:4, cut
                  +expr:1:5, cut
                    +side:1:5, cut
                    -side:1:5:Success(1:6, cut)
                    +side:1:7, cut
                    -side:1:7:Success(1:8, cut)
                  -expr:1:5:Success(1:8, cut)
                -side:1:4:Failure(side:1:4 / ")":1:8 ..."(2+3x))+4", cut)
              -expr:1:2:Failure(expr:1:2 / side:1:4 / ")":1:8 ..."1+(2+3x))+", cut)
            -side:1:1:Failure(side:1:1 / expr:1:2 / side:1:4 / ")":1:8 ..."(1+(2+3x))", cut)
          -expr:1:1:Failure(expr:1:1 / side:1:1 / expr:1:2 / side:1:4 / ")":1:8 ..."(1+(2+3x))", cut)
        """.lines.filter(_.trim != "").toSeq
        val minIndent = expected.map(_.takeWhile(_ == ' ').length).min
        val expectedString = expected.map(_.drop(minIndent)).mkString("\n")
        val capturedString = captured.mkString("\n")
        assert(capturedString == expectedString)
      }
    }
    'folding{
      sealed trait AndOr
      case object And extends AndOr
      case object Or extends AndOr
      def and[_: P] = P(IgnoreCase("And")).map(_ => And)
      def or[_: P] = P(IgnoreCase("Or")).map(_ => Or)
      def andOr[_: P] = P(and | or)

      def check(input: String, expectedOutput: String) = {
        val folded = parse(input, andOr(_)).fold(
          (_, _, _) => s"Cannot parse $input as an AndOr",
          (v, _) => s"Parsed: $v"
        )
        assert(folded == expectedOutput)
      }

      check("AnD", "Parsed: And")
      check("oR", "Parsed: Or")
      check("IllegalBooleanOperation", "Cannot parse IllegalBooleanOperation as an AndOr")
    }
  }
}
