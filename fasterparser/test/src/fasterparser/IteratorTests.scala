package test.fasterparser
import fasterparser._, Parsing._
import utest._

import scala.collection.mutable
object IteratorTests extends TestSuite {

  def toInput(string: String) = {

    class LoggedDropsParserInput(data: Iterator[String])
      extends IteratorParserInput(data) {

      val drops = mutable.SortedSet.empty[Int]

      override def dropBuffer(index: Int): Unit = {
        drops.add(index)
        super.dropBuffer(index)
      }

      override def toString = s"LoggedDropsParserInput($drops)"
    }
    new LoggedDropsParserInput(string.grouped(1).map(wrapString))
  }

  val tests = Tests {
    'basic - {
      import NoWhitespace._
      def p[_: P] = P( "ab" ~/ "cd".rep().! ~ "ef" | "z" )

      val Result.Success(res, i) = Parse.iter(Iterator("ab", "cd", "cd", "cd", "ef")).read(p(_))

      assert(res == "cdcdcd")
    }


    'immediateCutDrop - {
      import NoWhitespace._
      def p[_: P] = P( "ab" ~/ "cd" | "z" ).log

      val input = toInput("abcdef")
      val Result.Success(res, i) = Parse.input(input).read(p(_))
      // Make sure that we drop immediately at position 2, since that is where
      // the cut has taken place, rather than at position 4 as we did earlier.
      assert(input.drops == Set(2, 4))
    }
    'whitespaceImmediateCutDrop - {
      import NoWhitespace._
      implicit def whitespace(ctx: Parse[_]) = {
        implicit def ctx1 = ctx
        NoTrace(" ".? ~ " ".rep)
      }

      def p[_: P] = P( "ab" ~/ "cd" | "z" )

      val input = toInput("abcdef")
      val Result.Success(res, i) = Parse.input(input).read(p(_))
      // Make sure that we drop immediately at position 2, since that is where
      // the cut has taken place, rather than at position 4 as we did earlier.
      assert(input.drops == Set(2, 4))
    }
    'topLevelNoCuts - {
      import NoWhitespace._
      // Top-level sequences, which are not inside any `|`s or `.rep`s or `.?`s,
      // should dropBuffer immediately after every `~`, even without any cuts

      def p[_: P] = P( "a" ~ "b" ~ "c")
      def capt[_ : P] = P( p ~ p ~ p)
      val input = toInput("abcabcabc")
      val Result.Success(res, i) = Parse.input(input).read(capt(_))
      assert(input.drops == Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
    'cuts - {
      'capturing - {
        import NoWhitespace._

        def p[_: P] = P( "a" ~/ "b" ~/ "c")
        def capt[_: P] = P( p.! ~ p.! ~ p.!)
        val input = toInput("abcabcabc")
        val Result.Success(res, i) = Parse.input(input).read(capt(_))
        assert(
          i == 9,
          res == ("abc", "abc", "abc"),
          Set(3, 6, 9) == input.drops  // drops sequence elements
        )
      }

      'nocut - {
        import NoWhitespace._

        def p[_: P] = P( "a" ~/ "b" ~/ "c")
        def nocut[_: P] = P((NoCut(p) ~ NoCut(p) ~/ NoCut(p)) | "abcd")

        val input1 = toInput("abcabcabc")
        val Result.Success(_, i1) = Parse.input(input1).read(nocut(_))
        assert(
          i1 == 9,
          Set(6, 9) == input1.drops // drop non-droppable NoCut block after cut
        )

        val input2 = toInput("abcd")
        val Result.Success(_, i2) = Parse.input(input2).read(nocut(_))
        assert(
          i2 == 4,
          input2.drops.isEmpty // no drops during simple parsers, for instance literal
        )
      }

      'either - {
        import NoWhitespace._
        def p[_: P] = P( "a" ~ "b" ~ "c")
        def either[_: P] = P( (p ~ End) | ("abc" ~ p ~ End) | ("abcabc" ~ p ~ End))

        val input1 = toInput("abcabcabc")
        val Result.Success(_, i1) = Parse.input(input1).read(either(_))
        assert(
          i1 == 9,
          Set(6, 7, 8, 9) == input1.drops // drops in the last branch of Either
        )

        val input2 = toInput("abcabc")
        val Result.Success(_, i2) = Parse.input(input2).read(either(_))
        assert(
          i2 == 6,
          input2.drops.isEmpty // no drops at the end
        )

        val input3 = toInput("abc")
        val Result.Success(_, i3) = Parse.input(input3).read(either(_))
        assert(
          i3 == 3,
          input3.drops.isEmpty // no drops at the end
        )
      }

      'rep - {
        import NoWhitespace._
        def p[_: P] = P( "a" ~ "b" ~ "c")
        def rep[_: P] = P( (p.rep ~ "d") | (p.rep ~ "e") )

        val input1 = toInput("abcabcabcd")
        val Result.Success(_, i1) = Parse.input(input1).read(rep(_))
        assert(
          i1 == 10,
          input1.drops.isEmpty // no drops at the end of first branch
        )

        val input2 = toInput("abcabcabce")
        val Result.Success(_, i2) = Parse.input(input2).read(rep(_))

        assert(
          i1 == 10,
          Set(9, 10) == input2.drops // drops in the last branch
        )
      }

      'all - {
        import NoWhitespace._
        def p[_: P] = P( "a" ~ "b" ~ "c" ~/ "d")
        def np[_: P] = NoCut(p)
        def pp[_: P] = P( "a" ~ "b" ~ "c" ~ End)
        def all[_: P] = P( pp | (np ~/ np) | p ~ "e" | "abded".! )

        val input = toInput("abded")

        val Result.Success(res, _) = Parse.input(input).read(all(_))
        assert(
          res == "abded",
          input.drops.isEmpty // no drops in literal
        )
      }

      'whitespaceApi - {

        implicit def whitespace(ctx: Parse[_]): Parse[Unit] = {
          implicit def ctx1 = ctx
          NoTrace(" ".? ~~/ " ".repX) // note that the whitespace delimiter has cut
        }

        def a[_: P] = P( "aaa" )
        def b[_: P] = P( "bbb" )
        def ab[_: P] = P( a ~ b.? ~~ " " ~~ "ccc" )

        val input1 = toInput("aaa   bbb ccc")
        val Result.Success(_, i1) = Parse.input(input1).read(ab(_))
        val drops = input1.drops
        assert(
          i1 == 13,
          Set(3, 9, 10, 13) == drops// drops after a, b and at the end
          // no drops within the whitespaces, in spite of cut
        )

        val input2 = toInput("aaa ccc")
        val Result.Success(_, i2) = Parse.input(input2).read(ab(_))
        assert(
          i2 == 7,
          Set(3, 4, 7) == input2.drops // drops after a, whitespace and ccc
        )

        val input3 = toInput("aaa  ccc")
        // this shows behavior of whitespaceApi which requires quite tricky dropBuffer calls
        // it totally ignores first ~ and produces error in the second ~~
        assert(Parse.input(input3).read(ab(_)).isInstanceOf[Result.Failure])
      }

      'zeroDrops - {
        import NoWhitespace._
        def p[_: P] = P( (("big string, " ~ ("another string, " ~ ("a".? ~/ "b".?)) | "small string, ") ~ "end of input") | "some other input" )

        val input = toInput("big string, another string, end of input")
        val Result.Success(_, i) = Parse.input(input).read(p(_))
        val drops = input.drops
        assert(
          i == 40,
          Set(28) == drops
          // drops after "another string" because of the nested cut ~/, but not
          // after `"end of input" because cuts only apply to the `|` blocks
          // they are nested within
        )

      }
    }
    'traceFailure - {
      import NoWhitespace._
      def p[_: P] = P("[" ~ "]")

      Parse("[ ]").read(p(_)).asInstanceOf[Result.Failure].extra.traced
      val e = intercept[RuntimeException] {
        Parse.iter(Iterator("[", " ", "]")).read(p(_)).asInstanceOf[Result.Failure].extra.traced
      }
      assert(e.getMessage.contains("Cannot perform `.traced` on an `IteratorParserInput`"))
    }
  }
}