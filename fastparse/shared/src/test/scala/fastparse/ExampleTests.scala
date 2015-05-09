package fastparse


import utest._

/**
 * Demonstrates simulatneously parsing and
 * evaluating simple arithmetic expressions
 */
object ExampleTests extends TestSuite{

  val tests = TestSuite{
    'basic{
      'simple {
        import fastparse._
        val parseA = P( "a" )

        val Result.Success(value, successIndex) = parseA.parse("a")
        assert(value == (), successIndex == 1)

        val failure = parseA.parse("b").asInstanceOf[Result.Failure]
        assert(
          failure.parser == ("a": P0),
          failure.index == 0,
          failure.trace == """parseA:0 / "a":0 ..."b""""
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

        val abc = P( "a".rep("b") ~ "c").log("A")
        val Result.Success(_, 8) = abc.parse("abababac")

        val Result.Failure(parser, 3) = abc.parse("abaabac")
      }

      'option{
        val option = P( "c".? ~ "a".rep("b").! ~ End)
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


      'capturing{
        val capture1 = P( "a".rep.! ~ "b" ~ End)
        val Result.Success("aaa", 4) = capture1.parse("aaab")

        val capture2 = P( "a".rep.! ~ "b".! ~ End)
        val s @ Result.Success(("aaa", "b"), 4) = capture2.parse("aaab")

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
        val nocut = P( "val " ~ alpha.rep1.! | "def " ~ alpha.rep1.!)
        val Result.Success("abcd", _) = nocut.parse("val abcd")

        val failure = nocut.parse("val 1234").asInstanceOf[Result.Failure]
        assert(
          failure.index == 0,
          failure.trace ==
          """nocut:0 / (("val " ~ alpha.rep1) | ("def " ~ alpha.rep1)):0 ..."val 1234""""
        )
      }
      'withcut{
        val alpha = P( CharIn('a' to 'z') )
        val nocut = P( "val " ~! alpha.rep1.! | "def " ~! alpha.rep1.!)
        val Result.Success("abcd", _) = nocut.parse("val abcd")

        val failure = nocut.parse("val 1234").asInstanceOf[Result.Failure]
        assert(
          failure.index == 4,
          failure.trace ==
          """nocut:0 / alpha:4 / CharIn("abcdefghijklmnopqrstuvwxyz"):4 ..."1234""""
        )
      }
      'repnocut{
        val alpha = P( CharIn('a' to 'z') )
        val stmt = P( "val " ~ alpha.rep1.! ~ ";" ~ " ".rep )
        val stmts = P( stmt.rep1 ~ End )

        val Result.Success(Seq("abcd"), _) = stmts.parse("val abcd;")
        val Result.Success(Seq("abcd", "efg"), _) = stmts.parse("val abcd; val efg;")

        val failure = stmts.parse("val abcd; val ").asInstanceOf[Result.Failure]
        assert(
          failure.index == 10,
          failure.trace == """stmts:0 / End:10 ..."val """"
        )
      }
      'repcut{
        val alpha = P( CharIn('a' to 'z') )
        val stmt = P( "val " ~! alpha.rep1.! ~ ";" ~ " ".rep )
        val stmts = P( stmt.rep1 ~ End )

        val Result.Success(Seq("abcd"), _) = stmts.parse("val abcd;")
        val Result.Success(Seq("abcd", "efg"), _) = stmts.parse("val abcd; val efg;")

        val failure = stmts.parse("val abcd; val ").asInstanceOf[Result.Failure]
        assert(
          failure.index == 14,
          failure.trace ==
            """stmts:0 / stmt:10 / alpha:14 / CharIn("abcdefghijklmnopqrstuvwxyz"):14 ..."""""
        )
      }
      'delimiternocut{
        val digits = P( CharIn('0' to '9').rep1 )
        val tuple = P( "(" ~ digits.!.rep(",") ~ ")" )

        val Result.Success(Seq("1", "23"), _) = tuple.parse("(1,23)")

        val failure = tuple.parse("(1,)").asInstanceOf[Result.Failure]
        assert(
          failure.index == 2,
          failure.trace == """tuple:0 / ")":2 ...",)""""
        )
      }
      'delimitercut{
        val digits = P( CharIn('0' to '9').rep1 )
        val tuple = P( "(" ~ digits.!.rep("," ~! Pass) ~ ")" )

        val Result.Success(Seq("1", "23"), _) = tuple.parse("(1,23)")

        val failure = tuple.parse("(1,)").asInstanceOf[Result.Failure]
        assert(
          failure.index == 3,
          failure.trace == """tuple:0 / digits:3 / CharIn("0123456789"):3 ...")""""
        )
      }
    }
  }
}
