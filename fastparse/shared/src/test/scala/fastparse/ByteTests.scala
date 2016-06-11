package fastparse

import utest._

object ByteTests extends TestSuite {

  val tests = TestSuite {
    'basic {
      import fastparse.allByte._
      'simple {
        val parseA = P( BS(1) )

        val Parsed.Success(value, successIndex) = parseA.parse(BS(1))
        assert(value == (), successIndex == 1)

        val failure = parseA.parse(BS(2)).asInstanceOf[Parsed.Failure]
        println(failure.extra.traced.trace)
        assert(
          failure.lastParser == (BS(1): P0),
          failure.index == 0,
          failure.extra.traced.trace == """parseA:1:1 / "01":1:1 ..."02""""
        )
      }

        /*'sequence {
          val ab = P("a" ~ "b")

          val Parsed.Success(_, 2) = ab.parse("ab")

          val Parsed.Failure(parser, 1, _) = ab.parse("aa")
          assert(parser == ("b": P0))
        }
        'repeat {
          val ab = P("a".rep ~ "b")
          val Parsed.Success(_, 8) = ab.parse("aaaaaaab")
          val Parsed.Success(_, 4) = ab.parse("aaaba")

          val abc = P("a".rep(sep = "b") ~ "c")
          val Parsed.Success(_, 8) = abc.parse("abababac")
          val Parsed.Failure(parser, 3, _) = abc.parse("abaabac")

          val ab4 = P("a".rep(min = 2, max = 4, sep = "b"))
          val Parsed.Success(_, 7) = ab4.parse("ababababababa")

          val ab4c = P("a".rep(min = 2, max = 4, sep = "b") ~ "c")
          val Parsed.Failure(_, 1, _) = ab4c.parse("ac")
          val Parsed.Success(_, 4) = ab4c.parse("abac")
          val Parsed.Success(_, 8) = ab4c.parse("abababac")
          val Parsed.Failure(_, 7, _) = ab4c.parse("ababababac")
        }

        'option {
          val option = P("c".? ~ "a".rep(sep = "b").! ~ End)

          val Parsed.Success("aba", 3) = option.parse("aba")
          val Parsed.Success("aba", 3) = option.parse("aba")
        }

        'either {
          val either = P("a".rep ~ ("b" | "c" | "d") ~ End)

          val Parsed.Success(_, 6) = either.parse("aaaaab")
          val Parsed.Failure(parser, 5, _) = either.parse("aaaaae")
          assert(parser == ("b" | "c" | "d"))
        }*/
      }
    }
}
