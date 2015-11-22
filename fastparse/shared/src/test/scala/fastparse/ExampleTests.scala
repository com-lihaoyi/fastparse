package fastparse

import all._
import utest._

/**
 * Demonstrates simulatneously parsing and
 * evaluating simple arithmetic expressions
 */
object ExampleTests extends TestSuite{

  val tests = TestSuite{
    'basic{
      'simple {
        import fastparse.all._
        val parseA = P( "a" )

        val Result.Success(value, successIndex) = parseA.parse("a")
        assert(value == (), successIndex == 1)

        val failure = parseA.parse("b").asInstanceOf[Result.Failure]
        assert(
          failure.lastParser == ("a": P0),
          failure.index == 0,
          failure.traced.trace == """parseA:1:1 / "a":1:1 ..."b""""
        )
      }

      'sequence {
        val ab = P( "a" ~ "b" )

        val Result.Success(_, 2) = ab.parse("ab")

        val Result.Failure(parser, 1) = ab.parse("aa")
        assert(parser == ("b": P0))
      }
      'repeat{
        val ab = P( "a".rep ~ "b" )
        val Result.Success(_, 8) = ab.parse("aaaaaaab")
        val Result.Success(_, 4) = ab.parse("aaaba")

        val abc = P( "a".rep(sep="b") ~ "c")
        val Result.Success(_, 8) = abc.parse("abababac")
        val Result.Failure(parser, 3) = abc.parse("abaabac")

        val ab4 = P ( "a".rep(min=2, max=4, sep="b") )
        val Result.Success(_, 7) = ab4.parse("ababababababa")

        val ab4c = P ( "a".rep(min=2, max=4, sep="b") ~ "c" )
        val Result.Failure(_, 1) = ab4c.parse("ac")
        val Result.Success(_, 4) = ab4c.parse("abac")
        val Result.Success(_, 8) = ab4c.parse("abababac")
        val Result.Failure(_, 7) = ab4c.parse("ababababac")
      }

      'option{
        val option = P( "c".? ~ "a".rep(sep="b").! ~ End)

        val Result.Success("aba", 3) = option.parse("aba")
        val Result.Success("aba", 3) = option.parse("aba")
      }

      'either{
        val either = P( "a".rep ~ ("b" | "c" | "d") ~ End)

        val Result.Success(_, 6) = either.parse("aaaaab")
        val Result.Failure(parser, 5) = either.parse("aaaaae")
        assert(parser == ("b" | "c" | "d"))
      }


      'end{
        val noEnd = P( "a".rep ~ "b")
        val withEnd = P( "a".rep ~ "b" ~ End)

        val Result.Success(_, 4) = noEnd.parse("aaaba")
        val Result.Failure(End, 4) = withEnd.parse("aaaba")

      }
      'start{
        val ab = P( (("a" | Start) ~ "b").rep ~ End).!

        val Result.Success("abab", 4) = ab.parse("abab")
        val Result.Success("babab", 5) = ab.parse("babab")

        val Result.Failure(parser, 2) = ab.parse("abb")

      }

      'passfail{
        val Result.Success((), 0) = Pass.parse("asdad")
        val Result.Failure(Fail, 0) = Fail.parse("asdad")
      }

      'index{
        val finder = P( "hay".rep ~ Index ~ "needle" ~ "hay".rep )

        val Result.Success(9, _) = finder.parse("hayhayhayneedlehay")
      }

      'capturing{
        val capture1 = P( "a".rep.! ~ "b" ~ End)

        val Result.Success("aaa", 4) = capture1.parse("aaab")

        val capture2 = P( "a".rep.! ~ "b".! ~ End)

        val Result.Success(("aaa", "b"), 4) = capture2.parse("aaab")

        val capture3 = P( "a".rep.! ~ "b".! ~ "c".! ~ End)

        val Result.Success(("aaa", "b", "c"), 5) = capture3.parse("aaabc")

        val captureRep = P( "a".!.rep ~ "b" ~ End)

        val Result.Success(Seq("a", "a", "a"), 4) = captureRep.parse("aaab")

        val captureOpt = P( "a".rep ~ "b".!.? ~ End)

        val Result.Success(Some("b"), 4) = captureOpt.parse("aaab")
      }
      'anychar{
        val ab = P( "'" ~ AnyChar.! ~ "'" )

        val Result.Success("-", 3) = ab.parse("'-'")

        val Result.Failure(parser, 2) = ab.parse("'-='")
        assert(parser == ("'": P0))
      }


      'lookahead{
        val keyword = P( ("hello" ~ &(" ")).!.rep )

        val Result.Success(Seq("hello"), _) = keyword.parse("hello ")
        val Result.Success(Seq(), __) = keyword.parse("helloX")
      }
      'neglookahead{
        val keyword = P( "hello" ~ !" " ~ AnyChar ~ "world" ).!

        val Result.Success("hello-world", _) = keyword.parse("hello-world")
        val Result.Success("hello_world", _) = keyword.parse("hello_world")

        val Result.Failure(parser, 6) = keyword.parse("hello world")
        assert(parser == !(" "))
      }
      'map{
        val binary = P( ("0" | "1" ).rep.! )
        val binaryNum = P( binary.map(Integer.parseInt(_, 2)) )

        val Result.Success("1100", _) = binary.parse("1100")
        val Result.Success(12, _) = binaryNum.parse("1100")
      }
      'flatMap{
        val leftTag = P( "<" ~ (!">" ~ AnyChar).rep(1).! ~ ">" )
        def rightTag(s: String) = P( "</" ~ s.! ~ ">" )
        val xml = P( leftTag.flatMap(rightTag) )

        val Result.Success("a", _) = xml.parse("<a></a>")
        val Result.Success("abcde", _) = xml.parse("<abcde></abcde>")

        val failure = xml.parse("<abcde></edcba>").asInstanceOf[Result.Failure]
        assert(
          failure.traced.trace == """xml:1:1 / rightTag:1:8 / "abcde":1:10 ..."edcba>""""
        )
      }
      'filter{
        val digits = P(CharIn('0' to '9').rep(1).!).map(_.toInt)
        val even = digits.filter(_ % 2 == 0)
        val Result.Success(12, _) = even.parse("12")
        val failure = even.parse("123").asInstanceOf[Result.Failure]
        assert(even.toString == "digits.filter(<function1>)")
        assert(failure.traced.trace == "digits.filter(<function1>):1:1 ...\"123\"")
      }
      'opaque{
        val digit = CharIn('0' to '9')
        val letter = CharIn('A' to 'Z')
        def twice[T](p: Parser[T]) = p ~ p
        def errorMessage[T](p: Parser[T], str: String) =
          ParseError(p.parse(str).asInstanceOf[Result.Failure]).getMessage

        // Portuguese number plate format since 2006
        val numberPlate = P(twice(digit) ~ "-" ~ twice(letter) ~ "-" ~ twice(digit))

        assert(errorMessage(numberPlate, "11-A1-22") == """
          |found "1-22", expected CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZ") at index 4
          |11-A1-22
          |    ^""".stripMargin.trim)

        // Suppress implementation details from the error message
        val opaqueNumberPlate = numberPlate.opaque("<number-plate>")

        assert(errorMessage(opaqueNumberPlate, "11-A1-22") == """
          |found "11-A1-22", expected <number-plate> at index 0
          |11-A1-22
          |^""".stripMargin.trim)
      }
    }
    'charX{
      'charPred{
        val cp = P( CharPred(_.isUpper).rep.! ~ "." ~ End )

        val Result.Success("ABC", _) = cp.parse("ABC.")
        val Result.Failure(_, 2) = cp.parse("ABc.")
      }
      'charIn{
        val ci = P( CharIn("abc", "xyz").rep.! ~ End )

        val Result.Success("aaabbccxyz", _) = ci.parse("aaabbccxyz")
        val Result.Failure(_, 7) = ci.parse("aaabbccdxyz.")

        val digits = P( CharIn('0' to '9').rep.! )

        val Result.Success("12345", _) = digits.parse("12345abcde")
        val Result.Success("123", _) = digits.parse("123abcde45")
      }
      'charsWhile{
        val cw = P( CharsWhile(_ != ' ').! )

        val Result.Success("12345", _) = cw.parse("12345")
        val Result.Success("123", _) = cw.parse("123 45")
      }
      'stringIn{
        val si = P( StringIn("cow", "cattle").!.rep )

        val Result.Success(Seq("cow", "cattle"), _) = si.parse("cowcattle")
        val Result.Success(Seq("cow"), _) = si.parse("cowmoo")
      }
    }
    'cuts{
      'nocut{
        val alpha = P( CharIn('a' to 'z') )
        val nocut = P( "val " ~ alpha.rep(1).! | "def " ~ alpha.rep(1).!)

        val Result.Success("abcd", _) = nocut.parse("val abcd")

        val failure = nocut.parse("val 1234").asInstanceOf[Result.Failure]
        assert(
          failure.index == 0,
          failure.traced.trace ==
          """nocut:1:1 / ("val " ~ alpha.rep(1) | "def " ~ alpha.rep(1)):1:1 ..."val 1234""""
        )
      }
      'withcut{
        val alpha = P( CharIn('a' to 'z') )
        val nocut = P( "val " ~/ alpha.rep(1).! | "def " ~/ alpha.rep(1).!)

//        val Result.Success("abcd", _) = nocut.parse("val abcd")

        val failure = nocut.parse("val 1234").asInstanceOf[Result.Failure]
        assert(
          failure.index == 4,
          failure.traced.trace ==
          """nocut:1:1 / alpha:1:5 / CharIn("abcdefghijklmnopqrstuvwxyz"):1:5 ..."1234""""
        )
      }
      'repnocut{
        val alpha = P( CharIn('a' to 'z') )
        val stmt = P( "val " ~ alpha.rep(1).! ~ ";" ~ " ".rep )
        val stmts = P( stmt.rep(1) ~ End )

//        val Result.Success(Seq("abcd"), _) = stmts.parse("val abcd;")
//        val Result.Success(Seq("abcd", "efg"), _) = stmts.parse("val abcd; val efg;")
        val failure = stmts.parse("val abcd; val ").asInstanceOf[Result.Failure]
        assert(
          failure.index == 10,
          failure.traced.trace == """stmts:1:1 / (End | " "):1:11 ..."val """"
        )
      }
      'repcut{
        val alpha = P( CharIn('a' to 'z') )
        val stmt = P( "val " ~/ alpha.rep(1).! ~ ";" ~ " ".rep )
        val stmts = P( stmt.rep(1) ~ End )

        val Result.Success(Seq("abcd"), _) = stmts.parse("val abcd;")
        val Result.Success(Seq("abcd", "efg"), _) = stmts.parse("val abcd; val efg;")

        val failure = stmts.parse("val abcd; val ").asInstanceOf[Result.Failure]
        assert(
          failure.index == 14,
          failure.traced.trace ==
            """stmts:1:1 / stmt:1:11 / alpha:1:14 / CharIn("abcdefghijklmnopqrstuvwxyz"):1:14 ..."""""
        )
      }
      'delimiternocut{
        val digits = P( CharIn('0' to '9').rep(1) )
        val tuple = P( "(" ~ digits.!.rep(sep=",") ~ ")" )

        val Result.Success(Seq("1", "23"), _) = tuple.parse("(1,23)")

        val failure = tuple.parse("(1,)").asInstanceOf[Result.Failure]
        assert(
          failure.index == 2,
          failure.traced.trace == """tuple:1:1 / (")" | CharIn("0123456789")):1:3 ...",)""""
        )
      }
      'delimitercut{
        val digits = P( CharIn('0' to '9').rep(1) )
        val tuple = P( "(" ~ digits.!.rep(sep="," ~/ Pass) ~ ")" )

        val Result.Success(Seq("1", "23"), _) = tuple.parse("(1,23)")

        val failure = tuple.parse("(1,)").asInstanceOf[Result.Failure]
        assert(
          failure.index == 3,
          failure.traced.trace == """tuple:1:1 / digits:1:4 / CharIn("0123456789"):1:4 ...")""""
        )
      }
      'endcut{
        val digits = P( CharIn('0' to '9').rep(1) )
        val tuple = P( "(" ~ digits.!.rep(sep=",".~/) ~ ")" )

        val Result.Success(Seq("1", "23"), _) = tuple.parse("(1,23)")

        val failure = tuple.parse("(1,)").asInstanceOf[Result.Failure]
        assert(
          failure.index == 3,
          failure.traced.trace == """tuple:1:1 / digits:1:4 / CharIn("0123456789"):1:4 ...")""""
        )
      }
      'composecut{
         val digit = P( CharIn('0' to '9') )
         val time1 = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
         val time2 = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
         val Result.Success((), _) = time1.parse("12:30pm")
         val Result.Success((), _) = time2.parse("17:45")
         val time = P( time1 | time2 )
         val Result.Success((), _) = time.parse("12:30pm")
         val failure = time.parse("17:45").asInstanceOf[Result.Failure]
         assert(failure.index == 5)  // Expects am or pm
      }
      'composenocut{
         val digit = P( CharIn('0' to '9') )
         val time1 = P( ("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm") )
         val time2 = P( (("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit )
         val Result.Success((), _) = time1.parse("12:30pm")
         val Result.Success((), _) = time2.parse("17:45")
         val time = P( NoCut(time1) | time2 )
         val Result.Success((), _) = time.parse("12:30pm")
         val Result.Success((), _) = time.parse("17:45")
      }
    }
    'debugging{
      def check(a: Any, s: String) = assert(a.toString == s.trim)
      'original{
        object Foo{
          import fastparse.all._
          val plus = P( "+" )
          val num = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
          val side = P( "(" ~ expr ~ ")" | num )
          val expr: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}
        }


        check(
          Foo.expr.parse("(1+(2+3x))+4"),
          """Failure(("(" ~ expr ~ ")" | num):1:1 ..."(1+(2+3x))")"""
        )

      }
      'cuts{
        object Foo{
          import fastparse.all._
          val plus = P( "+" )
          val num = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
          val side = P( "(" ~/ expr ~ ")" | num )
          val expr: P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}
        }
        check(
          Foo.expr.parse("(1+(2+3x))+4"),
          """Failure(")":1:8 ..."x))+4")"""
        )
      }
      'log{
        val captured = collection.mutable.Buffer.empty[String]
        implicit val logger = Logger(captured.append(_))
        object Foo{
          import fastparse.all._
          val plus = P( "+" )
          val num = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
          val side = P( "(" ~/ expr ~ ")" | num ).log()
          val expr:P[Int] = P( side ~ plus ~ side ).map{case (l, r) => l + r}.log()
        }


        Foo.expr.parse("(1+(2+3x))+4")



        val expected = """
          +expr:0
            +side:0
              +expr:1
                +side:1
                -side:1:Success(2)
                +side:3
                  +expr:4
                    +side:4
                    -side:4:Success(5)
                    +side:6
                    -side:6:Success(7)
                  -expr:4:Success(7)
                -side:3:Failure(side:1:4 / ")":1:8 ..."(2+3x))+4", cut)
              -expr:1:Failure(expr:1:2 / side:1:4 / ")":1:8 ..."1+(2+3x))+", cut)
            -side:0:Failure(side:1:1 / expr:1:2 / side:1:4 / ")":1:8 ..."(1+(2+3x))", cut)
          -expr:0:Failure(expr:1:1 / side:1:1 / expr:1:2 / side:1:4 / ")":1:8 ..."(1+(2+3x))", cut)
        """.lines.filter(_.trim != "").toSeq
        val minIndent = expected.map(_.takeWhile(_ == ' ').length).min
        val expectedString = expected.map(_.drop(minIndent)).mkString("\n")
        val capturedString = captured.mkString("\n")
        assert(capturedString == expectedString)
      }
    }

  }
}
