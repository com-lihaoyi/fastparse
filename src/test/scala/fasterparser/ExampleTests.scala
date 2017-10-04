package fasterparser

import utest._
import Parse._
/**
  * Demonstrates simulatneously parsing and
  * evaluating simple arithmetic expressions
  */
object ExampleTests extends TestSuite{

  val tests = Tests{
    'basic{
      'simple {
        def parseA[_:Ctx] = P( "a" )

        val Parsed.Success(value, successIndex) = parseA("a")
        assert(value == (), successIndex == 1)

        val failure = parseA("b").asInstanceOf[Parsed.Failure]
        assert(
//          failure.lastParser == ("a": P0),
          failure.index == 0
//          failure.extra.traced.trace == """parseA:1:1 / "a":1:1 ..."b""""
        )
      }

      'sequence {
        def ab[_:Ctx] = P( "a" ~ "b" )

        val Parsed.Success(_, 2) = ab("ab")

        val Parsed.Failure(parser, 1, _) = ab("aa")
//        assert(parser == ("b": P0))
      }
      'repeat{
        def ab[_:Ctx] = P( "a".rep ~ "b" )
        val Parsed.Success(_, 8) = ab("aaaaaaab")
        val Parsed.Success(_, 4) = ab("aaaba")

        def abc[_:Ctx] = P( "a".rep(sep="b") ~ "c")
        val Parsed.Success(_, 8) = abc("abababac")
        val Parsed.Failure(parser, 3, _) = abc("abaabac")

        def ab4[_:Ctx] = P( "a".rep(min=2, max=4, sep="b") )
        val Parsed.Success(_, 7) = ab4("ababababababa")

        def ab2exactly[_:Ctx] = P( "ab".rep(exactly=2) )
        val Parsed.Success(_, 4) = ab2exactly("abab")

        def ab4c[_:Ctx] = P ( "a".rep(min=2, max=4, sep="b") ~ "c" )
        val Parsed.Failure(_, 1, _) = ab4c("ac")
        val Parsed.Success(_, 4) = ab4c("abac")
        val Parsed.Success(_, 8) = ab4c("abababac")
        val Parsed.Failure(_, 7, _) = ab4c("ababababac")
      }

      'option{
        def option[_:Ctx] = P( "c".? ~ "a".rep(sep="b").! ~ End)

        val Parsed.Success("aba", 3) = option("aba")
        val Parsed.Success("aba", 3) = option("aba")
      }

      'either{
        def either[_:Ctx] = P( "a".rep ~ ("b" | "c" | "d") ~ End)

        val Parsed.Success(_, 6) = either("aaaaab")
        val Parsed.Failure(parser, 5, _) = either("aaaaae")
//        assert(parser == ("b" | "c" | "d"))
      }


      'end{
        def noEnd[_:Ctx] = P( "a".rep ~ "b")
        def withEnd[_:Ctx] = P( "a".rep ~ "b" ~ End)

        val Parsed.Success(_, 4) = noEnd("aaaba")
        val Parsed.Failure(/*End*/ _, 4, _) = withEnd("aaaba")

      }
      'start{
        def ab[_:Ctx] = P( (("a" | Start) ~ "b").rep ~ End).!

        val Parsed.Success("abab", 4) = ab("abab")
        val Parsed.Success("babab", 5) = ab("babab")

        val Parsed.Failure(parser, 2, _) = ab("abb")

      }

      'passfail{
        val Parsed.Success((), 0) = Pass("asdad")
        val Parsed.Failure(/*Fail*/ _, 0, _) = Fail("asdad")
      }

      'index{
        def finder[_:Ctx] = P( "hay".rep ~ Index ~ "needle" ~ "hay".rep )

        val Parsed.Success(9, _) = finder("hayhayhayneedlehay")
      }

      'capturing{
        def capture1[_:Ctx] = P( "a".rep.! ~ "b" ~ End)

        val Parsed.Success("aaa", 4) = capture1("aaab")

        def capture2[_:Ctx] = P( "a".rep.! ~ "b".! ~ End)

        val Parsed.Success(("aaa", "b"), 4) = capture2("aaab")

        def capture3[_:Ctx] = P( "a".rep.! ~ "b".! ~ "c".! ~ End)

        val Parsed.Success(("aaa", "b", "c"), 5) = capture3("aaabc")

        def captureRep[_:Ctx] = P( "a".!.rep ~ "b" ~ End)

        val Parsed.Success(Seq("a", "a", "a"), 4) = captureRep("aaab")

        def captureOpt[_:Ctx] = P( "a".rep ~ "b".!.? ~ End)

        val Parsed.Success(Some("b"), 4) = captureOpt("aaab")
      }

      'anychar{
        def ab[_:Ctx] = P( "'" ~ AnyChar.! ~ "'" )

        val Parsed.Success("-", 3) = ab("'-'")

        val Parsed.Failure(parser, 2, _) = ab("'-='")
//        assert(parser == ("'": P0))
      }


      'lookahead{
        def keyword[_:Ctx] = P( ("hello" ~ &(" ")).!.rep )

        val Parsed.Success(Seq("hello"), _) = keyword("hello ")
        val Parsed.Success(Seq(), __) = keyword("helloX")
      }
      'neglookahead{
        def keyword[_:Ctx] = P( "hello" ~ !" " ~ AnyChar ~ "world" ).!

        val Parsed.Success("hello-world", _) = keyword("hello-world")
        val Parsed.Success("hello_world", _) = keyword("hello_world")

        val Parsed.Failure(parser, 5, _) = keyword("hello world")
//        assert(parser == !(" "))
      }
      'map{
        def binary[_:Ctx] = P( ("0" | "1" ).rep.! )
        def binaryNum[_:Ctx] = P( binary.map(Integer.parseInt(_, 2)) )

        val Parsed.Success("1100", _) = binary("1100")
        val Parsed.Success(12, _) = binaryNum("1100")
      }
      'flatMap{
        def leftTag[_:Ctx] = P( "<" ~ (!">" ~ AnyChar).rep(1).! ~ ">")
        def rightTag[_:Ctx](s: String) = P( "</" ~ s.! ~ ">" )
        def xml[_:Ctx] = P( leftTag.flatMap(rightTag) )

        val Parsed.Success("a", _) = xml("<a></a>")
        val Parsed.Success("abcde", _) = xml("<abcde></abcde>")

        val failure = xml("<abcde></edcba>").asInstanceOf[Parsed.Failure]
//        assert(
//          failure.extra.traced.trace == """xml:1:1 / rightTag:1:8 / "abcde":1:10 ..."edcba>""""
//        )
      }
      'flatMapFor{
        def leftTag[_:Ctx] = P( "<" ~ (!">" ~ AnyChar).rep(1).! ~ ">" )
        def rightTag[_:Ctx](s: String) = P( "</" ~ s.! ~ ">" )
        def xml[_:Ctx] = P(
          for{
            s <- leftTag
            right <- rightTag(s)
          } yield right
        )

        val Parsed.Success("a", _) = xml("<a></a>")
        val Parsed.Success("abcde", _) = xml("<abcde></abcde>")

        val failure = xml("<abcde></edcba>").asInstanceOf[Parsed.Failure]
//        assert(
//          failure.extra.traced.trace == """xml:1:1 / rightTag:1:8 / "abcde":1:10 ..."edcba>""""
//        )
      }
      'filter{
        def digits[_:Ctx] = P(CharIn('0' to '9').rep(1).!).map(_.toInt)
        def even[_:Ctx] = P( digits.filter(_ % 2 == 0) )
        val Parsed.Success(12, _) = even("12")
        val failure = even("123").asInstanceOf[Parsed.Failure]
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
//        def numberPlate[_:Ctx] = P(twice(digit) ~ "-" ~ twice(letter) ~ "-" ~ twice(digit))
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
        def cp[_:Ctx] = P( CharPred(_.isUpper).rep.! ~ "." ~ End )

        val Parsed.Success("ABC", _) = cp("ABC.")
        val Parsed.Failure(_, 2, _) = cp("ABc.")
      }
//      'charPredRaw{
//        def cp[_:Ctx] = P( CharPred.raw(_.isUpper).rep.! ~ "." ~ End )
//
//        val Parsed.Success("ABC", _) = cp("ABC.")
//        val Parsed.Failure(_, 2, _) = cp("ABc.")
//      }
      'charIn{
        def ci[_:Ctx] = P( CharIn("abc", "xyz").rep.! ~ End )

        val Parsed.Success("aaabbccxyz", _) = ci("aaabbccxyz")
        val Parsed.Failure(_, 7, _) = ci("aaabbccdxyz.")

        def digits[_:Ctx] = P( CharIn('0' to '9').rep.! )

        val Parsed.Success("12345", _) = digits("12345abcde")
        val Parsed.Success("123", _) = digits("123abcde45")
      }

      'charsWhile{
        def cw[_:Ctx] = P( CharsWhile(_ != ' ').! )

        val Parsed.Success("12345", _) = cw("12345")
        val Parsed.Success("123", _) = cw("123 45")
      }
      'charsWhileIn{
        def cw[_:Ctx] = P( CharsWhileIn("123456789").! )

        val Parsed.Success("12345", _) = cw("12345")
        val Parsed.Success("123", _) = cw("123 45")
      }
//      'charsWhileRaw{
//        def cw[_:Ctx] = P( CharsWhile.raw(_ != ' ').! )
//
//        val Parsed.Success("12345", _) = cw("12345")
//        val Parsed.Success("123", _) = cw("123 45")
//      }
      'stringIn{
//        def si[_:Ctx] = P( StringIn("cow", "cattle").!.rep )
//
//        val Parsed.Success(Seq("cow", "cattle"), _) = si("cowcattle")
//        val Parsed.Success(Seq("cow"), _) = si("cowmoo")
      }
    }
    'cuts{
      'nocut{
        def alpha[_:Ctx] = P( CharIn('a' to 'z') )
        def nocut[_:Ctx] = P( "val " ~ alpha.rep(1).! | "def " ~ alpha.rep(1).!)

        val Parsed.Success("abcd", _) = nocut("val abcd")

        val failure = nocut("val 1234").asInstanceOf[Parsed.Failure]
        assert(
          failure.index == 0//,
//          failure.extra.traced.trace ==
//            """nocut:1:1 / ("val " ~ alpha.rep(1) | "def " ~ alpha.rep(1)):1:1 ..."val 1234""""
        )
      }
      'withcut{
        def alpha[_:Ctx] = P( CharIn('a' to 'z') )
        def nocut[_:Ctx] = P( "val " ~/ alpha.rep(1).! | "def " ~/ alpha.rep(1).!)

        val Parsed.Success("abcd", _) = nocut("val abcd")

        val failure = nocut("val 1234").asInstanceOf[Parsed.Failure]
        assert(
          failure.index == 4//,
//          failure.extra.traced.trace ==
//            """nocut:1:1 / alpha:1:5 / CharIn("abcdefghijklmnopqrstuvwxyz"):1:5 ..."1234""""
        )
      }
      'repnocut{
        def alpha[_:Ctx] = P( CharIn('a' to 'z') )
        def stmt[_:Ctx] = P( "val " ~ alpha.rep(1).! ~ ";" ~ " ".rep )
        def stmts[_:Ctx] = P( stmt.rep(1) ~ End )

        val Parsed.Success(Seq("abcd"), _) = stmts("val abcd;")
        val Parsed.Success(Seq("abcd", "efg"), _) = stmts("val abcd; val efg;")

        val failure = stmts("val abcd; val ").asInstanceOf[Parsed.Failure]
        assert(
          failure.index == 10//,
//          failure.extra.traced.trace == """stmts:1:1 / (End | " "):1:11 ..."val """"
        )
      }
      'repcut{
        def alpha[_:Ctx] = P( CharIn('a' to 'z') )
        def stmt[_:Ctx] = P( "val " ~/ alpha.rep(1).! ~ ";" ~ " ".rep )
        def stmts[_:Ctx] = P( stmt.rep(1) ~ End )

        val Parsed.Success(Seq("abcd"), _) = stmts("val abcd;")
        val Parsed.Success(Seq("abcd", "efg"), _) = stmts("val abcd; val efg;")

        val failure = stmts("val abcd; val ").asInstanceOf[Parsed.Failure]
        assert(
          failure.index == 14//,
//          failure.extra.traced.trace ==
//            """stmts:1:1 / stmt:1:11 / alpha:1:15 / CharIn("abcdefghijklmnopqrstuvwxyz"):1:15 ..."""""
        )
      }
      'delimiternocut{
        def digits[_:Ctx] = P( CharIn('0' to '9').rep(1) )
        def tuple[_:Ctx] = P( "(" ~ digits.!.rep(sep=",") ~ ")" )

        val Parsed.Success(Seq("1", "23"), _) = tuple("(1,23)")

        val failure = tuple("(1,)").asInstanceOf[Parsed.Failure]
        assert(
          failure.index == 2//,
//          failure.extra.traced.trace == """tuple:1:1 / (")" | CharIn("0123456789")):1:3 ...",)""""
        )
      }
      'delimitercut{
        def digits[_:Ctx] = P( CharIn('0' to '9').rep(1) )
        def tuple[_:Ctx] = P( "(" ~ digits.!.rep(sep=("," ~/ Pass)) ~ ")" )

        val Parsed.Success(Seq("1", "23"), _) = tuple("(1,23)")

        val failure = tuple("(1,)").asInstanceOf[Parsed.Failure]
        assert(
          failure.index == 3//,
//          failure.extra.traced.trace == """tuple:1:1 / digits:1:4 / CharIn("0123456789"):1:4 ...")""""
        )
      }
      'endcut{
        def digits[_:Ctx] = P( CharIn('0' to '9').rep(1) )
        def tuple[_:Ctx] = P( "(" ~ digits.!.rep(sep=",".~/) ~ ")" )

        val Parsed.Success(Seq("1", "23"), _) = tuple("(1,23)")

        val failure = tuple("(1,)").asInstanceOf[Parsed.Failure]
//        val trace = failure.extra.traced.trace
        assert(
          failure.index == 3//,
//          trace == """tuple:1:1 / digits:1:4 / CharIn("0123456789"):1:4 ...")""""
        )
      }
      'composecut{
        def digit[_:Ctx] = P( CharIn('0' to '9') )
        def time1[_:Ctx] = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
        def time2[_:Ctx] = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
        val Parsed.Success((), _) = time1("12:30pm")
        val Parsed.Success((), _) = time2("17:45")
        def time[_:Ctx] = P( time1 | time2 ).log
        val Parsed.Success((), _) = time("12:30pm")
        val failure = time("17:45").asInstanceOf[Parsed.Failure]
        assert(failure.index == 5)  // Expects am or pm
      }
      'composenocut{
//        def digit[_:Ctx] = P( CharIn('0' to '9') )
//        def time1[_:Ctx] = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
//        def time2[_:Ctx] = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
//        val Parsed.Success((), _) = time1("12:30pm")
//        val Parsed.Success((), _) = time2("17:45")
//        def time[_:Ctx] = P( NoCut(time1) | time2 )
//        val Parsed.Success((), _) = time("12:30pm")
//        val Parsed.Success((), _) = time("17:45")
      }
    }
    'debugging{
      def check(a: Any, s: String) = assert(a.toString == s.trim)
      'original{
//        object Foo{
//
//          def plus[_:Ctx] = P( "+" )
//          def num[_:Ctx] = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
//          def side[_:Ctx] = P( "(" ~ expr ~ ")" | num )
//          def expr[_:Ctx]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}
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
//          def plus[_:Ctx] = P( "+" )
//          def num[_:Ctx] = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
//          def side[_:Ctx] = P( "(" ~/ expr ~ ")" | num )
//          def expr[_:Ctx]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}
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
//          def plus[_:Ctx] = P( "+" )
//          def num[_:Ctx] = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
//          def side[_:Ctx] = P( "(" ~/ expr ~ ")" | num ).log
//          def expr[_:Ctx]: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}.log
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
//      def and[_:Ctx] = P(IgnoreCase("And")).map(_ => And)
//      def or[_:Ctx] = P(IgnoreCase("Or")).map(_ => Or)
//      def andOr[_:Ctx] = P(and | or)
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
