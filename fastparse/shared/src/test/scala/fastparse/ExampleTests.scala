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
        val Result.Success(value, successIndex, _) = a.parse("a")
        assert(value == ())
        assert(successIndex == 1)


        val Result.Failure(input, fullStack, failureIndex, parser, _) = a.parse("b")
        assert(parser == ("a": R0))
      }

      'sequence {
        val ab = R( "a" ~ "b" )
        val Result.Success(_, successIndex, _) = ab.parse("ab")
        assert(successIndex == 2)

        val Result.Failure(_, _, failureIndex, parser, _) = ab.parse("aa")
        assert(
          failureIndex == 1,
          parser == ("b": R0)
        )
      }
      'repeat{
        val ab = R( "a".rep ~ "b" )
        val Result.Success(_, successIndex, _) = ab.parse("aaaaaaab")
        assert(successIndex == 8)

        val Result.Success(_, successIndex2, _) = ab.parse("aaaba")
        assert(successIndex2 == 4)

        val abc = R( "a".rep("b") ~ "c").log("A")
        val Result.Success(_, successIndex3, _) = abc.parse("abababac")
        assert(successIndex3 == 8)

        val Result.Failure(_, _, failureIndex, parser, _) = abc.parse("abaabac")
        assert(failureIndex == 3)
      }
      'end{
        val ab = R( "a".rep ~ "b" ~ End)
        val Result.Failure(_, _, failureIndex, parser, _) = ab.parse("aaaba")
        assert(
          failureIndex == 4,
          parser == End
        )
      }
      'start{
        val ab = R( (("a" | Start) ~ "b").rep ~ End).!
        val Result.Success("abab", _, _) = ab.parse("abab")
        val Result.Success("babab", _, _) = ab.parse("babab")

        val Result.Failure(_, _, 2, _, _) = ab.parse("abb")

      }


      'capturing{
        val capture = R( "a".rep.! ~ "b" ~ End)
        val Result.Success("aaa", _, _) = capture.parse("aaab")

        val capture2 = R( "a".rep.! ~ "b".! ~ End)
        val s @ Result.Success(("aaa", "b"), _, _) = capture2.parse("aaab")

        val capture3 = R( "a".rep.! ~ "b".! ~ "c".! ~ End)
        val Result.Success(("aaa", "b", "c"), _, _) = capture3.parse("aaabc")

      }
      'anychar{
        val ab = R( "'" ~ AnyChar.! ~ "'" )
        val Result.Success("-", 3, _) = ab.parse("'-'")
        val Result.Failure(_, _, 2, parser, _) = ab.parse("'-='")
        assert(parser == ("'": R0))
      }
      'option{
        val option = R( "c".? ~ "a".rep("b").! ~ End)
        val Result.Success("aba", _, _) = option.parse("aba")
        val Result.Success("aba", _, _) = option.parse("aba")
      }
      'optrepcapture{
        val capture = R( "c".!.? ~ "a".!.rep("b") ~ End)
        val Result.Success((Some("c"), Seq("a", "a")), _, _) = capture.parse("caba")
      }
      'either{
        val either = R( "a".rep ~ ("b" | "c" | "d") ~ End)
        val Result.Failure(_, _, failureIndex, parser, _) = either.parse("aaaaae")
        assert(
          failureIndex == 5,
          parser == ("b" | "c" | "d")
        )
      }
      'lookahead{
        val keyword = R( ("hello" ~ &(" ")).!.rep )
        val Result.Success(Seq("hello"), _, _) = keyword.parse("hello ")
        val Result.Success(Seq(), _, _) = keyword.parse("helloX")
      }
      'neglookahead{
        val keyword = R( "hello" ~ !" " ~ AnyChar ~ "world" ).!
        val Result.Success("hello-world", _, _) = keyword.parse("hello-world")
        val Result.Success("hello_world", _, _) = keyword.parse("hello_world")
        val Result.Failure(_, _, 6, parser, _) = keyword.parse("hello world")
        assert(parser == !(" "))
      }
      'map{
        val binary = R( ("0" | "1" ).rep.! )
        val binaryNum = R( binary.map(Integer.parseInt(_, 2)) )
        val Result.Success("1100", _, _) = binary.parse("1100")
        val Result.Success(12, _, _) = binaryNum.parse("1100")
      }
    }
    'charX{
      'charPred{
        val cp = R( CharPred(_.isUpper).rep.! ~ "." ~ End )
        val Result.Success("ABC", _, _) = cp.parse("ABC.")
        val Result.Failure(_, _, 2, _, _) = cp.parse("ABc.")
      }
      'charIn{
        val ci = R( CharIn("abc", "xyz").rep.! ~ End )
        val Result.Success("aaabbccxyz", _, _) = ci.parse("aaabbccxyz")
        val Result.Failure(_, _, 7, _, _) = ci.parse("aaabbccdxyz.")

        val digits = R( CharIn('0' to '9').rep.! )
        val Result.Success("12345", _, _) = digits.parse("12345abcde")
        val Result.Success("123", _, _) = digits.parse("123abcde45")
      }
      'charswhile{
        val cw = R( CharsWhile(_ != ' ').! )
        val Result.Success("12345", _, _) = cw.parse("12345")
        val Result.Success("123", _, _) = cw.parse("123 45")
      }
      'stringIn{
        val si = R( StringIn("cow", "cattle").!.rep )
        val Result.Success(Seq("cow", "cattle"), _, _) = si.parse("cowcattle")
        val Result.Success(Seq("cow"), _, _) = si.parse("cowmoo")
      }
    }
  }
}
