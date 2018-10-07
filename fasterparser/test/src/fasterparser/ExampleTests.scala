package test.scala.fasterparser

import fasterparser._
import utest._
import fasterparser.Parsing._
/**
  * Demonstrates simulatneously parsing and
  * evaluating simple arithmetic expressions
  */
object ExampleTests extends TestSuite{
  implicit def whitespace(cfg: Parse[_]): Parse[Unit] = Pass(cfg)
  val tests = Tests{
    'basic{
      'simple {
        def parseA[_: P] = P( "a" )

        val Result.Success(value, successIndex) = parseA(Parse("a")).result
        assert(value == (), successIndex == 1)

        val failure = parseA(Parse("b")).result.asInstanceOf[Result.Failure]
        assert(
//          failure.lastParser == ("a": P0),
          failure.index == 0
//          failure.extra.traced.trace == """parseA:1:1 / "a":1:1 ..."b""""
        )
      }

      'sequence {
        def ab[_: P] = P( EagerOpsStr("a").~[Unit, Unit](LiteralStr("b")(implicitly)) )

        val Result.Success(_, 2) = ab(Parse("ab")).result

        val Result.Failure(1, _) = ab(Parse("aa")).result
      }
      'repeat{
        def ab[_: P] = P( "a".rep ~ "b" )
        val Result.Success(_, 8) = ab(Parse("aaaaaaab")).result
        val Result.Success(_, 4) = ab(Parse("aaaba")).result

        def abc[_: P] = P( "a".rep(sep="b") ~ "c")
        val Result.Success(_, 8) = abc(Parse("abababac")).result
        val Result.Failure(3, _) = abc(Parse("abaabac")).result

        def ab4[_: P] = P( "a".rep(min=2, max=4, sep="b") )
        val Result.Success(_, 7) = ab4(Parse("ababababababa")).result

        def ab2exactly[_: P] = P( "ab".rep(exactly=2) )
        val Result.Success(_, 4) = ab2exactly(Parse("abab")).result

        def ab4c[_: P] = P ( "a".rep(min=2, max=4, sep="b") ~ "c" )
        val Result.Failure(1, _) = ab4c(Parse("ac")).result
        val Result.Success(_, 4) = ab4c(Parse("abac")).result
        val Result.Success(_, 8) = ab4c(Parse("abababac")).result
        val Result.Failure(7, _) = ab4c(Parse("ababababac")).result
      }

      'option{
        def option[_: P] = P( "c".? ~ "a".rep(sep="b").! ~ End)

        val Result.Success("aba", 3) = option(Parse("aba")).result
        val Result.Success("aba", 3) = option(Parse("aba")).result
      }

      'either{
        def either[_: P] = P( "a".rep ~ ("b" | "c" | "d") ~ End)

        val Result.Success(_, 6) = either(Parse("aaaaab")).result
        val Result.Failure(5, _) = either(Parse("aaaaae")).result
//        assert(parser == ("b" | "c" | "d"))
      }


      'end{
        def noEnd[_: P] = P( "a".rep ~ "b")
        def withEnd[_: P] = P( "a".rep ~ "b" ~ End)

        val Result.Success(_, 4) = noEnd(Parse("aaaba")).result
        val Result.Failure(4, _) = withEnd(Parse("aaaba")).result

      }
      'start{
        def ab[_: P] = P( (("a" | Start) ~ "b").rep ~ End).!

        val Result.Success("abab", 4) = ab(Parse("abab")).result
        val Result.Success("babab", 5) = ab(Parse("babab")).result

        val Result.Failure(2, _) = ab(Parse("abb")).result

      }

      'passfail{
        val Result.Success((), 0) = Pass(Parse("asdad")).result
        val Result.Failure(0, _) = Fail(Parse("asdad")).result
      }

      'index{
        def finder[_: P] = P( "hay".rep ~ Index ~ "needle" ~ "hay".rep )

        val Result.Success(9, _) = finder(Parse("hayhayhayneedlehay")).result
      }

      'capturing{
        def capture1[_: P] = P( "a".rep.! ~ "b" ~ End)

        val Result.Success("aaa", 4) = capture1(Parse("aaab")).result

        def capture2[_: P] = P( "a".rep.! ~ "b".! ~ End)

        val Result.Success(("aaa", "b"), 4) = capture2(Parse("aaab")).result

        def capture3[_: P] = P( "a".rep.! ~ "b".! ~ "c".! ~ End)

        val Result.Success(("aaa", "b", "c"), 5) = capture3(Parse("aaabc")).result

        def captureRep[_: P] = P( "a".!.rep ~ "b" ~ End)

        val Result.Success(Seq("a", "a", "a"), 4) = captureRep(Parse("aaab")).result

        def captureOpt[_: P] = P( "a".rep ~ "b".!.? ~ End)

        val Result.Success(Some("b"), 4) = captureOpt(Parse("aaab")).result
      }

      'anychar{
        def ab[_: P] = P( "'" ~ AnyChar.! ~ "'" )

        val Result.Success("-", 3) = ab(Parse("'-'")).result

        val Result.Failure(2, _) = ab(Parse("'-='")).result
//        assert(parser == ("'": P0))
      }


      'lookahead{
        def keyword[_: P] = P( ("hello" ~ &(" ")).!.rep )

        val Result.Success(Seq("hello"), _) = keyword(Parse("hello ")).result
        val Result.Success(Seq(), __) = keyword(Parse("helloX")).result
      }
      'neglookahead{
        def keyword[_: P] = P( "hello" ~ !" " ~ AnyChar ~ "world" ).!

        val Result.Success("hello-world", _) = keyword(Parse("hello-world")).result
        val Result.Success("hello_world", _) = keyword(Parse("hello_world")).result

        val Result.Failure(5, _) = keyword(Parse("hello world")).result
//        assert(parser == !(" "))
      }
      'map{
        def binary[_: P] = P( ("0" | "1" ).rep.! )
        def binaryNum[_: P] = P( binary.map(Integer.parseInt(_, 2)) )

        val Result.Success("1100", _) = binary(Parse("1100")).result
        val Result.Success(12, _) = binaryNum(Parse("1100")).result
      }
      'flatMap{
        def leftTag[_: P] = P( "<" ~ (!">" ~ AnyChar).rep(1).! ~ ">")
        def rightTag[_: P](s: String) = P( "</" ~ s.! ~ ">" )
        def xml[_: P] = P( leftTag.flatMap(rightTag) )

        val Result.Success("a", _) = xml(Parse("<a></a>")).result
        val Result.Success("abcde", _) = xml(Parse("<abcde></abcde>")).result

        val failure = xml(Parse("<abcde></edcba>")).result.asInstanceOf[Result.Failure]
//        assert(
//          failure.extra.traced.trace == """xml:1:1 / rightTag:1:8 / "abcde":1:10 ..."edcba>""""
//        )
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

        val Result.Success("a", _) = xml(Parse("<a></a>")).result
        val Result.Success("abcde", _) = xml(Parse("<abcde></abcde>")).result

        val failure = xml(Parse("<abcde></edcba>")).result.asInstanceOf[Result.Failure]
//        assert(
//          failure.extra.traced.trace == """xml:1:1 / rightTag:1:8 / "abcde":1:10 ..."edcba>""""
//        )
      }
      'filter{

        def digits[_: P] = P(CharPred(c => '0' <= c && c <= '9').rep(1).!).map(_.toInt)
        def even[_: P] = P( digits.filter(_ % 2 == 0) )
        val Result.Success(12, _) = even(Parse("12")).result
        val failure = even(Parse("123")).result.asInstanceOf[Result.Failure]
//        assert("""digits.filter\(.*\)$""".r.findPrefixOf(even.toString).isDefined)
//        assert("""digits.filter\(.*\):1:1 ..."123"$""".r.findPrefixOf(failure.extra.traced.trace).isDefined)
      }
      'opaque{
//        val digit = CharIn('0' to '9')
//        val letter = CharIn('A' to 'Z')
//        def twice[T](p: Parser[T]) = p ~ p
//        def errorMessage[T](p: Parser[T], str: String) =
//          ParseError(p(str).asInstanceOf[Parsed.Failure]).getMessage
//
//        // Portuguese number plate format since 2006
//        def numberPlate[_: P] = P(twice(digit) ~ "-" ~ twice(letter) ~ "-" ~ twice(digit))
//
//        assert(errorMessage(numberPlate, "11-A1-22") == """
//                                                          |found "1-22", expected CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZ") at index 4
//                                                          |11-A1-22
//                                                          |    ^""".stripMargin.trim)
//
//        // Suppress implementation details from the error message
//        val opaqueNumberPlate = numberPlate.opaque("<number-plate>")
//
//        assert(errorMessage(opaqueNumberPlate, "11-A1-22") == """
//                                                                |found "11-A1-22", expected <number-plate> at index 0
//                                                                |11-A1-22
//                                                                |^""".stripMargin.trim)
      }
    }
    'charX{
      'charPred{
        def cp[_: P] = P( CharPred(_.isUpper).rep.! ~ "." ~ End )

        val Result.Success("ABC", _) = cp(Parse("ABC.")).result
        val Result.Failure(2, _) = cp(Parse("ABc.")).result
      }
//      'charPredRaw{
//        def cp[_: P] = P( CharPred.raw(_.isUpper).rep.! ~ "." ~ End )
//
//        val Parsed.Success("ABC", _) = cp("ABC.")
//        val Parsed.Failure(_, 2, _) = cp("ABc.")
//      }
      'charIn{
        val chars = "abcxyz".toSet
        def ci[_: P] = P( CharPred(chars).rep.! ~ End )

        val Result.Success("aaabbccxyz", _) = ci(Parse("aaabbccxyz")).result
        val Result.Failure(7, _) = ci(Parse("aaabbccdxyz.")).result

        val digitChars = ('0' to '9').toSet
        def digits[_: P] = P( CharPred(digitChars).rep.! )

        val Result.Success("12345", _) = digits(Parse("12345abcde")).result
        val Result.Success("123", _) = digits(Parse("123abcde45")).result
      }

      'charsWhile{
        def cw[_: P] = P( CharsWhile(_ != ' ').! )

        val Result.Success("12345", _) = cw(Parse("12345")).result
        val Result.Success("123", _) = cw(Parse("123 45")).result
      }
      'charsWhileIn{
        val digits = "123456789".toSet
        def cw[_: P] = P( CharsWhile(digits).! )

        val Result.Success("12345", _) = cw(Parse("12345")).result
        val Result.Success("123", _) = cw(Parse("123 45")).result
      }
//      'charsWhileRaw{
//        def cw[_: P] = P( CharsWhile.raw(_ != ' ').! )
//
//        val Parsed.Success("12345", _) = cw("12345")
//        val Parsed.Success("123", _) = cw("123 45")
//      }
      'stringIn{
//        def si[_: P] = P( StringIn("cow", "cattle").!.rep )
//
//        val Parsed.Success(Seq("cow", "cattle"), _) = si("cowcattle")
//        val Parsed.Success(Seq("cow"), _) = si("cowmoo")
      }
    }
    'cuts{
      'nocut{
        def alpha[_: P] = P( CharPred(c => 'a' <= c && c <= 'z') )
        def nocut[_: P] = P( "val " ~ alpha.rep(1).! | "def " ~ alpha.rep(1).!)

        val Result.Success("abcd", _) = nocut(Parse("val abcd")).result

        val failure = nocut(Parse("val 1234")).result.asInstanceOf[Result.Failure]
        assert(
          failure.index == 0//,
//          failure.extra.traced.trace ==
//            """nocut:1:1 / ("val " ~ alpha.rep(1) | "def " ~ alpha.rep(1)):1:1 ..."val 1234""""
        )
      }
      'withcut{
        def alpha[_: P] = P( CharPred(c => 'a' <= c && c <= 'z') )
        def nocut[_: P] = P( "val " ~/ alpha.rep(1).! | "def " ~/ alpha.rep(1).!)

        val Result.Success("abcd", _) = nocut(Parse("val abcd")).result

        val failure = nocut(Parse("val 1234")).result.asInstanceOf[Result.Failure]
        assert(
          failure.index == 4//,
//          failure.extra.traced.trace ==
//            """nocut:1:1 / alpha:1:5 / CharIn("abcdefghijklmnopqrstuvwxyz"):1:5 ..."1234""""
        )
      }
      'repnocut{
        def alpha[_: P] = P( CharPred(c => 'a' <= c && c <= 'z') )
        def stmt[_: P] = P( "val " ~ alpha.rep(1).! ~ ";" ~ " ".rep )
        def stmts[_: P] = P( stmt.rep(1) ~ End )

        val Result.Success(Seq("abcd"), _) = stmts(Parse("val abcd;")).result
        val Result.Success(Seq("abcd", "efg"), _) = stmts(Parse("val abcd; val efg;")).result

        val failure = stmts(Parse("val abcd; val ")).result.asInstanceOf[Result.Failure]
        assert(
          failure.index == 10//,
//          failure.extra.traced.trace == """stmts:1:1 / (End | " "):1:11 ..."val """"
        )
      }
      'repcut{
        def alpha[_: P] = P( CharPred(c => 'a' <= c && c <= 'z') )
        def stmt[_: P] = P( "val " ~/ alpha.rep(1).! ~ ";" ~ " ".rep )
        def stmts[_: P] = P( stmt.rep(1) ~ End )

        val Result.Success(Seq("abcd"), _) = stmts(Parse("val abcd;")).result
        val Result.Success(Seq("abcd", "efg"), _) = stmts(Parse("val abcd; val efg;")).result

        val failure = stmts(Parse("val abcd; val ")).result.asInstanceOf[Result.Failure]
        assert(
          failure.index == 14//,
//          failure.extra.traced.trace ==
//            """stmts:1:1 / stmt:1:11 / alpha:1:15 / CharIn("abcdefghijklmnopqrstuvwxyz"):1:15 ..."""""
        )
      }
      'delimiternocut{
        def digits[_: P] = P( CharPred(c => '0' <= c && c <= '9').rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=",") ~ ")" )

        val Result.Success(Seq("1", "23"), _) = tuple(Parse("(1,23)")).result

        val failure = tuple(Parse("(1,)")).result.asInstanceOf[Result.Failure]
        assert(
          failure.index == 2//,
//          failure.extra.traced.trace == """tuple:1:1 / (")" | CharIn("0123456789")):1:3 ...",)""""
        )
      }
      'delimitercut{
        def digits[_: P] = P( CharPred(c => '0' <= c && c <= '9').rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=("," ~/ Pass)) ~ ")" )

        val Result.Success(Seq("1", "23"), _) = tuple(Parse("(1,23)")).result

        val failure = tuple(Parse("(1,)")).result.asInstanceOf[Result.Failure]
        assert(
          failure.index == 3//,
//          failure.extra.traced.trace == """tuple:1:1 / digits:1:4 / CharIn("0123456789"):1:4 ...")""""
        )
      }
      'endcut{
        def digits[_: P] = P( CharPred(c => '0' <= c && c <= '9').rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=","./) ~ ")" )

        val Result.Success(Seq("1", "23"), _) = tuple(Parse("(1,23)")).result

        val failure = tuple(Parse("(1,)")).result.asInstanceOf[Result.Failure]
//        val trace = failure.extra.traced.trace
        assert(
          failure.index == 3//,
//          trace == """tuple:1:1 / digits:1:4 / CharIn("0123456789"):1:4 ...")""""
        )
      }
      'composecut{
        def digit[_: P] = P( CharPred(c => '0' <= c && c <= '9') )
        def time1[_: P] = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
        def time2[_: P] = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
        val Result.Success((), _) = time1(Parse("12:30pm")).result
        val Result.Success((), _) = time2(Parse("17:45")).result
        def time[_: P] = P( time1 | time2 ).log
        val Result.Success((), _) = time(Parse("12:30pm")).result
        val failure = time(Parse("17:45")).result.asInstanceOf[Result.Failure]
        assert(failure.index == 5)  // Expects am or pm
      }
      'composenocut{
//        def digit[_: P] = P( CharIn('0' to '9') )
//        def time1[_: P] = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
//        def time2[_: P] = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
//        val Parsed.Success((), _) = time1("12:30pm")
//        val Parsed.Success((), _) = time2("17:45")
//        def time[_: P] = P( NoCut(time1) | time2 )
//        val Parsed.Success((), _) = time("12:30pm")
//        val Parsed.Success((), _) = time("17:45")
      }
    }
    'debugging{
      def check(a: Any, s: String) = assert(a.toString == s.trim)
      'original{
//        object Foo{
//
//          def plus[_: P] = P( "+" )
//          def num[_: P] = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
//          def side[_: P] = P( "(" ~ expr ~ ")" | num )
//          def expr[_: P]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}
//        }
//
//
//        check(
//          Foo.expr("(1+(2+3x))+4"),
//          """Failure(("(" ~ expr ~ ")" | num):1:1 ..."(1+(2+3x))")"""
//        )

      }
      'cuts{
//        object Foo{
//
//          def plus[_: P] = P( "+" )
//          def num[_: P] = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
//          def side[_: P] = P( "(" ~/ expr ~ ")" | num )
//          def expr[_: P]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}
//        }
//        check(
//          Foo.expr("(1+(2+3x))+4"),
//          """Failure(")":1:8 ..."x))+4")"""
//        )
      }
      'log{
//        val captured = collection.mutable.Buffer.empty[String]
//        implicit val logger = Logger(captured.append(_))
//        object Foo{
//
//          def plus[_: P] = P( "+" )
//          def num[_: P] = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
//          def side[_: P] = P( "(" ~/ expr ~ ")" | num ).log
//          def expr[_: P]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}.log
//        }
//
//
//        Foo.expr("(1+(2+3x))+4")
//
//
//
//        val expected = """
//          +expr:1:1
//            +side:1:1
//              +expr:1:2
//                +side:1:2
//                -side:1:2:Success(1:3)
//                +side:1:4
//                  +expr:1:5
//                    +side:1:5
//                    -side:1:5:Success(1:6)
//                    +side:1:7
//                    -side:1:7:Success(1:8)
//                  -expr:1:5:Success(1:8)
//                -side:1:4:Failure(side:1:4 / ")":1:8 ..."(2+3x))+4", cut)
//              -expr:1:2:Failure(expr:1:2 / side:1:4 / ")":1:8 ..."1+(2+3x))+", cut)
//            -side:1:1:Failure(side:1:1 / expr:1:2 / side:1:4 / ")":1:8 ..."(1+(2+3x))", cut)
//          -expr:1:1:Failure(expr:1:1 / side:1:1 / expr:1:2 / side:1:4 / ")":1:8 ..."(1+(2+3x))", cut)
//        """.lines.filter(_.trim != "").toSeq
//        val minIndent = expected.map(_.takeWhile(_ == ' ').length).min
//        val expectedString = expected.map(_.drop(minIndent)).mkString("\n")
//        val capturedString = captured.mkString("\n")
//        assert(capturedString == expectedString)
      }
    }
    'folding{
//      sealed trait AndOr
//      case object And extends AndOr
//      case object Or extends AndOr
//      def and[_: P] = P(IgnoreCase("And")).map(_ => And)
//      def or[_: P] = P(IgnoreCase("Or")).map(_ => Or)
//      def andOr[_: P] = P(and | or)
//
//      def check(input: String, expectedOutput: String) =
//        assert(andOr(input).fold((_, _, _) => s"Cannot parse $input as an AndOr", (v, _) => s"Parsed: $v") == expectedOutput)
//
//      check("AnD", "Parsed: And")
//      check("oR", "Parsed: Or")
//      check("IllegalBooleanOperation", "Cannot parse IllegalBooleanOperation as an AndOr")
    }

  }
}
