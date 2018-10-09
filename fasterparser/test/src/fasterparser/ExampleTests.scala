package test.fasterparser

import fasterparser._
import utest._
import fasterparser.Parsing._
/**
  * Demonstrates simulatneously parsing and
  * evaluating simple arithmetic expressions
  */
object ExampleTests extends TestSuite{
  import fasterparser.NoWhitespace._
  val tests = Tests{
    'basic{
      'simple {
        def parseA[_: P] = P( "a" )

        val Result.Success(value, successIndex) = Parse("a").read(parseA(_))
        assert(value == (), successIndex == 1)

        val failure = Parse("b").read(parseA(_)).asInstanceOf[Result.Failure]
        val trace = failure.traced.trace
        assert(
          failure.stack == List(("\"a\"", 0)),
          failure.toString == """Result.Failure(Expected "a":1:1, found "b")""",
          trace == """Expected parseA:1:1 / "a":1:1, found "b""""
        )
      }

      'sequence {
        def ab[_: P] = P( EagerOpsStr("a").~[Unit, Unit](LiteralStr("b")(implicitly)) )

        val Result.Success(_, 2) = Parse("ab").read(ab(_))

        val Result.Failure(1, _, _) = Parse("aa").read(ab(_))
      }
      'repeat{
        def ab[_: P] = P( "a".rep ~ "b" )
        val Result.Success(_, 8) = Parse("aaaaaaab").read(ab(_))
        val Result.Success(_, 4) = Parse("aaaba").read(ab(_))

        def abc[_: P] = P( "a".rep(sep="b") ~ "c")
        val Result.Success(_, 8) = Parse("abababac").read(abc(_))
        val Result.Failure(3, _, _) = Parse("abaabac").read(abc(_))

        def ab4[_: P] = P( "a".rep(min=2, max=4, sep="b") )
        val Result.Success(_, 7) = Parse("ababababababa").read(ab4(_))

        def ab2exactly[_: P] = P( "ab".rep(exactly=2) )
        val Result.Success(_, 4) = Parse("abab").read(ab2exactly(_))

        def ab4c[_: P] = P ( "a".rep(min=2, max=4, sep="b") ~ "c" )
        val Result.Failure(1, _, _) = Parse("ac").read(ab4c(_))
        val Result.Success(_, 4) = Parse("abac").read(ab4c(_))
        val Result.Success(_, 8) = Parse("abababac").read(ab4c(_))
        val Result.Failure(7, _, _) = Parse("ababababac").read(ab4c(_))
      }

      'option{
        def option[_: P] = P( "c".? ~ "a".rep(sep="b").! ~ End)

        val Result.Success("aba", 3) = Parse("aba").read(option(_))
        val Result.Success("aba", 3) = Parse("aba").read(option(_))
      }

      'either{
        def either[_: P] = P( "a".rep ~ ("b" | "c" | "d") ~ End)

        val Result.Success(_, 6) = Parse("aaaaab").read(either(_))
        val f @ Result.Failure(5, _, _) = Parse("aaaaae").read(either(_))
        val trace = f.traced.trace
        assert(
          f.toString == """Result.Failure(Expected ???:1:6, found "e")""",
          trace == """Expected either:1:1 / "a" | "b" | "c" | "d":1:6, found "e""""
        )
      }


      'end{
        def noEnd[_: P] = P( "a".rep ~ "b")
        def withEnd[_: P] = P( "a".rep ~ "b" ~ End)

        val Result.Success(_, 4) = Parse("aaaba").read(noEnd(_))
        val Result.Failure(4, _, _) = Parse("aaaba").read(withEnd(_))

      }
      'start{
        def ab[_: P] = P( (("a" | Start) ~ "b").rep ~ End).!

        val Result.Success("abab", 4) = Parse("abab").read(ab(_))
        val Result.Success("babab", 5) = Parse("babab").read(ab(_))

        val Result.Failure(2, _, _) = Parse("abb").read(ab(_))

      }

      'passfail{
        val Result.Success((), 0) = Parse("asdad").read(Pass(_))
        val Result.Failure(0, _, _) = Parse("asdad").read(Fail(_))
      }

      'index{
        def finder[_: P] = P( "hay".rep ~ Index ~ "needle" ~ "hay".rep )

        val Result.Success(9, _) = Parse("hayhayhayneedlehay").read(finder(_))
      }

      'capturing{
        def capture1[_: P] = P( "a".rep.! ~ "b" ~ End)

        val Result.Success("aaa", 4) = Parse("aaab").read(capture1(_))

        def capture2[_: P] = P( "a".rep.! ~ "b".! ~ End)

        val Result.Success(("aaa", "b"), 4) = Parse("aaab").read(capture2(_))

        def capture3[_: P] = P( "a".rep.! ~ "b".! ~ "c".! ~ End)

        val Result.Success(("aaa", "b", "c"), 5) = Parse("aaabc").read(capture3(_))

        def captureRep[_: P] = P( "a".!.rep ~ "b" ~ End)

        val Result.Success(Seq("a", "a", "a"), 4) = Parse("aaab").read(captureRep(_))

        def captureOpt[_: P] = P( "a".rep ~ "b".!.? ~ End)

        val Result.Success(Some("b"), 4) = Parse("aaab").read(captureOpt(_))
      }

      'anychar{
        def ab[_: P] = P( "'" ~ AnyChar.! ~ "'" )

        val Result.Success("-", 3) = Parse("'-'").read(ab(_))

        val f @ Result.Failure(2, _, _) = Parse("'-='").read(ab(_))
        assert(f.stack.head._1 == "\"'\"")
      }


      'lookahead{
        def keyword[_: P] = P( ("hello" ~ &(" ")).!.rep )

        val Result.Success(Seq("hello"), _) = Parse("hello ").read(keyword(_))
        val Result.Success(Seq(), __) = Parse("helloX").read(keyword(_))
      }
      'neglookahead{
        def keyword[_: P] = P( "hello" ~ !" " ~ AnyChar ~ "world" ).!

        val Result.Success("hello-world", _) = Parse("hello-world").read(keyword(_))
        val Result.Success("hello_world", _) = Parse("hello_world").read(keyword(_))

        val Result.Failure(5, _, _) = Parse("hello world").read(keyword(_))
//        assert(parser == !(" "))
      }
      'map{
        def binary[_: P] = P( ("0" | "1" ).rep.! )
        def binaryNum[_: P] = P( binary.map(Integer.parseInt(_, 2)) )

        val Result.Success("1100", _) = Parse("1100").read(binary(_))
        val Result.Success(12, _) = Parse("1100").read(binaryNum(_))
      }
      'flatMap{
        def leftTag[_: P] = P( "<" ~ (!">" ~ AnyChar).rep(1).! ~ ">")
        def rightTag[_: P](s: String) = P( "</" ~ s.! ~ ">" )
        def xml[_: P] = P( leftTag.flatMap(rightTag) )

        val Result.Success("a", _) = Parse("<a></a>").read(xml(_))
        val Result.Success("abcde", _) = Parse("<abcde></abcde>").read(xml(_))

        val failure = Parse("<abcde></edcba>").read(xml(_)).asInstanceOf[Result.Failure]
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

        val Result.Success("a", _) = Parse("<a></a>").read(xml(_))
        val Result.Success("abcde", _) = Parse("<abcde></abcde>").read(xml(_))

        val failure = Parse("<abcde></edcba>").read(xml(_)).asInstanceOf[Result.Failure]
        assert(
          failure.extra.traced.trace == """Expected xml:1:1 / rightTag:1:8 / "abcde":1:10, found "edcba>""""
        )
      }
      'filter{

        def digits[_: P] = P(CharPred(c => '0' <= c && c <= '9').rep(1).!).map(_.toInt)
        def even[_: P] = P( digits.filter(_ % 2 == 0) )
        val Result.Success(12, _) = Parse("12").read(even(_))
        val failure = Parse("123").read(even(_)).asInstanceOf[Result.Failure]
      }
      'opaque{
        def digit[_: P] = CharIn("0-9")
        def letter[_: P] = CharIn("A-Z")
        def twice[T](p: => P[T]) = p ~ p
        def errorMessage[T](p: P[_] => P[T], str: String) =
          Parse(str).read(p).asInstanceOf[Result.Failure].trace

        // Portuguese number plate format since 2006
        def numberPlate[_: P] = P(twice(digit) ~ "-" ~ twice(letter) ~ "-" ~ twice(digit))

        val err1 = errorMessage(numberPlate(_), "11-A1-22")
        assert(err1 == """Expected [A-Z]:1:5, found "1-22"""")

        // Suppress implementation details from the error message
        def opaqueNumberPlate[_: P] = numberPlate.opaque("<number-plate>")

        val err2 = errorMessage(opaqueNumberPlate(_), "11-A1-22")
        assert(err2 == """Expected <number-plate>:1:1, found "11-A1-22"""")
      }
    }
    'charX{
      'charPred{
        def cp[_: P] = P( CharPred(_.isUpper).rep.! ~ "." ~ End )

        val Result.Success("ABC", _) = Parse("ABC.").read(cp(_))
        val Result.Failure(2, _, _) = Parse("ABc.").read(cp(_))
      }

      'charIn{
        def ci[_: P] = P( CharIn("abc", "xyz").rep.! ~ End )

        val Result.Success("aaabbccxyz", _) = Parse("aaabbccxyz").read(ci(_))
        val Result.Failure(7, _, _) = Parse("aaabbccdxyz.").read(ci(_))


        def digits[_: P] = P( CharIn("0-9").rep.! )

        val Result.Success("12345", _) = Parse("12345abcde").read(digits(_))
        val Result.Success("123", _) = Parse("123abcde45").read(digits(_))
      }

      'charsWhile{
        def cw[_: P] = P( CharsWhile(_ != ' ').! )

        val Result.Success("12345", _) = Parse("12345").read(cw(_))
        val Result.Success("123", _) = Parse("123 45").read(cw(_))
      }
      'charsWhileIn{

        def cw[_: P] = P( CharsWhileIn("123456789").! )

        val Result.Success("12345", _) = Parse("12345").read(cw(_))
        val Result.Success("123", _) = Parse("123 45").read(cw(_))
      }

      'stringIn{
        def si[_: P] = P( StringIn("cow", "cattle").!.rep )

        val Result.Success(Seq("cow", "cattle"), _) = Parse("cowcattle").read(si(_))
        val Result.Success(Seq("cow"), _) = Parse("cowmoo").read(si(_))

        def si2[_: P] = P( StringIn("abstract", "case", "catch", "class", "def", "do", "else",
          "extends", "false", "finally", "final", "finally", "forSome",
          "for", "if", "implicit", "import", "lazy", "match", "new",
          "null", "object", "override", "package", "private", "protected",
          "return", "sealed", "super", "this", "throw", "trait", "try",
          "true", "type", "val", "var", "while", "with", "yield", "_", "macro").!.rep )
      }
    }
    'cuts{
      'nocut{
        def alpha[_: P] = P( CharIn("a-z") )
        def nocut[_: P] = P( "val " ~ alpha.rep(1).! | "def " ~ alpha.rep(1).!)

        val Result.Success("abcd", _) = Parse("val abcd").read(nocut(_))

        val failure = Parse("val 1234").read(nocut(_)).asInstanceOf[Result.Failure]
        assert(
          failure.index == 0,
          failure.extra.traced.trace ==
            """Expected nocut:1:1 / "val " | "def ":1:1, found "val 1234""""
        )
      }
      'withcut{
        def alpha[_: P] = P( CharIn("a-z") )
        def nocut[_: P] = P( "val " ~/ alpha.rep(1).! | "def " ~/ alpha.rep(1).!)

        val Result.Success("abcd", _) = Parse("val abcd").read(nocut(_))

        val failure = Parse("val 1234").read(nocut(_)).asInstanceOf[Result.Failure]
        assert(
          failure.index == 4,
          failure.extra.traced.trace =="""Expected nocut:1:1 / alpha:1:5 / [a-z]:1:5, found "1234""""
        )
      }
      'repnocut{
        def alpha[_: P] = P( CharIn("a-z") )
        def stmt[_: P] = P( "val " ~ alpha.rep(1).! ~ ";" ~ " ".rep )
        def stmts[_: P] = P( stmt.rep(1) ~ End )

        val Result.Success(Seq("abcd"), _) = Parse("val abcd;").read(stmts(_))
        val Result.Success(Seq("abcd", "efg"), _) = Parse("val abcd; val efg;").read(stmts(_))

        val failure = Parse("val abcd; val ").read(stmts(_)).asInstanceOf[Result.Failure]
        assert(
          failure.index == 10,
          failure.extra.traced.trace == """Expected stmts:1:1 / " " | "val " | end-of-input:1:11, found "val """"
        )
      }
      'repcut{
        def alpha[_: P] = P( CharIn("a-z") )
        def stmt[_: P] = P( "val " ~/ alpha.rep(1).! ~ ";" ~ " ".rep )
        def stmts[_: P] = P( stmt.rep(1) ~ End )

        val Result.Success(Seq("abcd"), _) = Parse("val abcd;").read(stmts(_))
        val Result.Success(Seq("abcd", "efg"), _) = Parse("val abcd; val efg;").read(stmts(_))

        val failure = Parse("val abcd; val ").read(stmts(_)).asInstanceOf[Result.Failure]
        assert(
          failure.index == 14,
          failure.extra.traced.trace ==
            """Expected stmts:1:1 / stmt:1:11 / alpha:1:15 / [a-z]:1:15, found """""
        )
      }
      'delimiternocut{
        def digits[_: P] = P( CharIn("0-9").rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=",") ~ ")" )

        val Result.Success(Seq("1", "23"), _) = Parse("(1,23)").read(tuple(_))

        val failure = Parse("(1,)").read(tuple(_)).asInstanceOf[Result.Failure]
        assert(
          failure.index == 2,
          failure.extra.traced.trace == """Expected tuple:1:1 / [0-9] | "," | ")":1:3, found ",)""""
        )
      }
      'delimitercut{
        def digits[_: P] = P( CharIn("0-9").rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=","./) ~ ")" )

        val Result.Success(Seq("1", "23"), _) = Parse("(1,23)").read(tuple(_))

        val failure = Parse("(1,)").read(tuple(_)).asInstanceOf[Result.Failure]
        assert(
          failure.index == 3,
          failure.extra.traced.trace == """Expected tuple:1:1 / digits:1:4 / [0-9]:1:4, found ")""""
        )
      }
      'endcut{
        def digits[_: P] = P( CharIn("0-9").rep(1) )
        def tuple[_: P] = P( "(" ~ digits.!.rep(sep=","./) ~ ")" )

        val Result.Success(Seq("1", "23"), _) = Parse("(1,23)").read(tuple(_))

        val failure = Parse("(1,)").read(tuple(_)).asInstanceOf[Result.Failure]
        assert(
          failure.index == 3,
          failure.extra.traced.trace == """Expected tuple:1:1 / digits:1:4 / [0-9]:1:4, found ")""""
        )

      }
      'composecut{
        def digit[_: P] = P( CharIn("0-9") )
        def time1[_: P] = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
        def time2[_: P] = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
        val Result.Success((), _) = Parse("12:30pm").read(time1(_))
        val Result.Success((), _) = Parse("17:45").read(time2(_))
        def time[_: P] = P( time1 | time2 ).log
        val Result.Success((), _) = Parse("12:30pm").read(time(_))
        val failure = Parse("17:45").read(time(_)).asInstanceOf[Result.Failure]
        assert(failure.index == 5)  // Expects am or pm
      }
      'composenocut{
        def digit[_: P] = P( CharIn("0-9") )
        def time1[_: P] = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
        def time2[_: P] = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
        val Result.Success((), _) = Parse("12:30pm").read(time1(_))
        val Result.Success((), _) = Parse("17:45").read(time2(_))
        def time[_: P] = P( NoCut(time1) | time2 )
        val Result.Success((), _) = Parse("12:30pm").read(time(_))
        val Result.Success((), _) = Parse("17:45").read(time(_))
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
          Parse("(1+(2+3x))+4").read(Foo.expr(_)),
          """Result.Failure(Expected ???:1:1, found "(1+(2+3x))")"""
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
          Parse("(1+(2+3x))+4").read(Foo.expr(_)),
          """Result.Failure(Expected ")":1:8, found "x))+4")"""
        )
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


        Parse("(1+(2+3x))+4").read(Foo.expr(_))



        val expected = """
          +expr:1:1
            +side:1:1
              +expr:1:2
                +side:1:2
                -side:1:2:Success(1:3)
                +side:1:4
                  +expr:1:5
                    +side:1:5
                    -side:1:5:Success(1:6)
                    +side:1:7
                    -side:1:7:Success(1:8)
                  -expr:1:5:Success(1:8)
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
        val folded = andOr(Parse(input)).result.fold(
          (_, _) => s"Cannot parse $input as an AndOr",
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
