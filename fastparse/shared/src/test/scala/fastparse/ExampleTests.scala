package fastparse

import fastparse.Parser.CharsWhile
import utest._

/**
 * Demonstrates simulatneously parsing and
 * evaluating simple arithmetic expressions
 */
object ExampleTests extends TestSuite{

  val tests = TestSuite{
    'basic{
      'simple {
        val a = R( "a" )
        val Result.Success(value, successIndex) = a.parse("a")
        assert(value == (), successIndex == 1)


        val Result.Failure(parser, failureIndex) = a.parse("b")
        assert(parser == ("a": R0), failureIndex == 0)
      }

      'sequence {
        val ab = R( "a" ~ "b" )
        val Result.Success(_, 2) = ab.parse("ab")

        val Result.Failure(parser, 1) = ab.parse("aa")
        assert(parser == ("b": R0))
      }
      'repeat{
        val ab = R( "a".rep ~ "b" )
        val Result.Success(_, 8) = ab.parse("aaaaaaab")

        val Result.Success(_, 4) = ab.parse("aaaba")

        val abc = R( "a".rep("b") ~ "c").log("A")
        val Result.Success(_, 8) = abc.parse("abababac")

        val Result.Failure(parser, 3) = abc.parse("abaabac")
      }
      'end{
        val ab = R( "a".rep ~ "b" ~ End)
        val Result.Failure(End, 4) = ab.parse("aaaba")

      }
      'start{
        val ab = R( (("a" | Start) ~ "b").rep ~ End).!
        val Result.Success("abab", 4) = ab.parse("abab")
        val Result.Success("babab", 5) = ab.parse("babab")

        val Result.Failure(parser, 2) = ab.parse("abb")

      }


      'capturing{
        val capture = R( "a".rep.! ~ "b" ~ End)
        val Result.Success("aaa", 4) = capture.parse("aaab")

        val capture2 = R( "a".rep.! ~ "b".! ~ End)
        val s @ Result.Success(("aaa", "b"), 4) = capture2.parse("aaab")

        val capture3 = R( "a".rep.! ~ "b".! ~ "c".! ~ End)
        val Result.Success(("aaa", "b", "c"), 5) = capture3.parse("aaabc")

      }
      'anychar{
        val ab = R( "'" ~ AnyChar.! ~ "'" )
        val Result.Success("-", 3) = ab.parse("'-'")
        val Result.Failure(parser, 2) = ab.parse("'-='")
        assert(parser == ("'": R0))
      }
      'option{
        val option = R( "c".? ~ "a".rep("b").! ~ End)
        val Result.Success("aba", 3) = option.parse("aba")
        val Result.Success("aba", 3) = option.parse("aba")
      }
      'optrepcapture{
        val capture = R( "c".!.? ~ "a".!.rep("b") ~ End)
        val Result.Success((Some("c"), Seq("a", "a")), 4) = capture.parse("caba")
      }
      'either{
        val either = R( "a".rep ~ ("b" | "c" | "d") ~ End)
        val Result.Failure(parser, 5) = either.parse("aaaaae")
        assert(
          parser == ("b" | "c" | "d")
        )
      }
      'lookahead{
        val keyword = R( ("hello" ~ &(" ")).!.rep )
        val Result.Success(Seq("hello"), _) = keyword.parse("hello ")
        val Result.Success(Seq(), __) = keyword.parse("helloX")
      }
      'neglookahead{
        val keyword = R( "hello" ~ !" " ~ AnyChar ~ "world" ).!
        val Result.Success("hello-world", _) = keyword.parse("hello-world")
        val Result.Success("hello_world", _) = keyword.parse("hello_world")
        val Result.Failure(parser, 6) = keyword.parse("hello world")
        assert(parser == !(" "))
      }
      'map{
        val binary = R( ("0" | "1" ).rep.! )
        val binaryNum = R( binary.map(Integer.parseInt(_, 2)) )
        val Result.Success("1100", _) = binary.parse("1100")
        val Result.Success(12, _) = binaryNum.parse("1100")
      }
    }
    'charX{
      'charPred{
        val cp = R( CharPred(_.isUpper).rep.! ~ "." ~ End )
        val Result.Success("ABC", _) = cp.parse("ABC.")
        val Result.Failure(_, 2) = cp.parse("ABc.")
      }
      'charIn{
        val ci = R( CharIn("abc", "xyz").rep.! ~ End )
        val Result.Success("aaabbccxyz", _) = ci.parse("aaabbccxyz")
        val Result.Failure(_, 7) = ci.parse("aaabbccdxyz.")

        val digits = R( CharIn('0' to '9').rep.! )
        val Result.Success("12345", _) = digits.parse("12345abcde")
        val Result.Success("123", _) = digits.parse("123abcde45")
      }
      'charswhile{
        val cw = R( CharsWhile(_ != ' ').! )
        val Result.Success("12345", _) = cw.parse("12345")
        val Result.Success("123", _) = cw.parse("123 45")
      }
      'stringIn{
        val si = R( StringIn("cow", "cattle").!.rep )
        val Result.Success(Seq("cow", "cattle"), _) = si.parse("cowcattle")
        val Result.Success(Seq("cow"), _) = si.parse("cowmoo")
      }
    }
  }
}
