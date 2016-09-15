package fastparse

import utest._

import scala.collection.mutable
import scala.reflect.ClassTag

object IteratorTests extends TestSuite {



  def toInput(string: String) = {
    import fastparse.all._
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

  val tests = TestSuite {

    'basic{
      import fastparse.all._
      val p = P( "ab" ~/ "cd".rep().! ~ "ef" | "z" )

      val Parsed.Success(res, i) = p.parseIterator(
        Iterator("ab", "cd", "cd", "cd", "ef")
      )
      
      assert(res == "cdcdcd")
    }

    'immediateCutDrop{
      import fastparse.all._
      val p = P( "ab" ~/ "cd" | "z" )

      val input = toInput("abcdef")
      val Parsed.Success(res, i) = p.parseInput(input)
      // Make sure that we drop immediately at position 2, since that is where
      // the cut has taken place, rather than at position 4 as we did earlier.
      assert(input.drops == Set(2, 4))
    }
    'whitespaceImmediateCutDrop{
      import fastparse.noApi._
      val White = fastparse.WhitespaceApi.Wrapper{
        import fastparse.all._
        NoTrace(" ".? ~ " ".rep)
      }
      import White._
      val p = P( "ab" ~/ "cd" | "z" )

      val input = toInput("abcdef")
      val Parsed.Success(res, i) = p.parseInput(input)
      // Make sure that we drop immediately at position 2, since that is where
      // the cut has taken place, rather than at position 4 as we did earlier.
      assert(input.drops == Set(2, 4))
    }
    'topLevelNoCuts{
      // Top-level sequences, which are not inside any `|`s or `.rep`s or `.?`s,
      // should dropBuffer immediately after every `~`, even without any cuts
      import fastparse.all._

      val p = P( "a" ~ "b" ~ "c")
      val capt = P( p ~ p ~ p)
      val input = toInput("abcabcabc")
      val Parsed.Success(res, i) = capt.parseInput(input)
      assert(input.drops == Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
    'cuts {
      'capturing {
        import fastparse.all._

        val p = P( "a" ~/ "b" ~/ "c")
        val capt = P( p.! ~ p.! ~ p.!)
        val input = toInput("abcabcabc")
        val Parsed.Success(res, i) = capt.parseInput(input)
        assert(
          i == 9,
          res == ("abc", "abc", "abc"),
          Set(3, 6, 9) == input.drops  // drops sequence elements
        )
      }

      'nocut {
        import fastparse.all._

        val p = P( "a" ~/ "b" ~/ "c")
        val nocut = P((NoCut(p) ~ NoCut(p) ~/ NoCut(p)) | "abcd")

        val input1 = toInput("abcabcabc")
        val Parsed.Success(_, i1) = nocut.parseInput(input1)
        assert(
          i1 == 9,
          Set(6, 9) == input1.drops // drop non-droppable NoCut block after cut
        )

        val input2 = toInput("abcd")
        val Parsed.Success(_, i2) = nocut.parseInput(input2)
        assert(
          i2 == 4,
          input2.drops.isEmpty // no drops during simple parsers, for instance literal
        )
      }

      'either {
        import fastparse.all._

        val p = P( "a" ~ "b" ~ "c")
        val either = P( (p ~ End) | ("abc" ~ p ~ End) | ("abcabc" ~ p ~ End))

        val input1 = toInput("abcabcabc")
        val Parsed.Success(_, i1) = either.parseInput(input1)
        assert(
          i1 == 9,
          Set(6, 7, 8, 9) == input1.drops // drops in the last branch of Either
        )

        val input2 = toInput("abcabc")
        val Parsed.Success(_, i2) = either.parseInput(input2)
        assert(
          i2 == 6,
          input2.drops.isEmpty // no drops at the end
        )

        val input3 = toInput("abc")
        val Parsed.Success(_, i3) = either.parseInput(input3)
        assert(
          i3 == 3,
          input3.drops.isEmpty // no drops at the end
        )
      }

      'rep {
        import fastparse.all._

        val p = P( "a" ~ "b" ~ "c")
        val rep = P( (p.rep ~ "d") | (p.rep ~ "e") )

        val input1 = toInput("abcabcabcd")
        val Parsed.Success(_, i1) = rep.parseInput(input1)
        assert(
          i1 == 10,
          input1.drops.isEmpty // no drops at the end of first branch
        )

        val input2 = toInput("abcabcabce")
        val Parsed.Success(_, i2) = rep.parseInput(input2)

        assert(
          i1 == 10,
          Set(9, 10) == input2.drops // drops in the last branch
        )
      }

      'all {
        import fastparse.all._

        val p = P( "a" ~ "b" ~ "c" ~/ "d")
        val np = NoCut(p)
        val pp = P( "a" ~ "b" ~ "c" ~ End)

        val all = P( pp | (np ~/ np) | p ~ "e" | "abded".! )

        val input = toInput("abded")

        val Parsed.Success(res, _) = all.parseInput(input)
        assert(
          res == "abded",
          input.drops.isEmpty // no drops in literal
        )
      }

      'whitespaceApi {
        import fastparse.noApi._
        val White = fastparse.WhitespaceApi.Wrapper{
          import fastparse.all._
          NoTrace(" ".? ~/ " ".rep) // note that the whitespace delimiter has cut
        }
        import White._

        val a = P( "aaa" )
        val b = P( "bbb" )
        val ab = P( a ~ b.? ~~ " " ~~ "ccc" )

        val input1 = toInput("aaa   bbb ccc")
        val Parsed.Success(_, i1) = ab.parseInput(input1)
        assert(
          i1 == 13,
          Set(3, 9, 10, 13) == input1.drops // drops after a, b and at the end
                                            // no drops within the whitespaces, in spite of cut
        )

        val input2 = toInput("aaa ccc")
        val Parsed.Success(_, i2) = ab.parseInput(input2)
        assert(
          i2 == 7,
          Set(3, 4, 7) == input2.drops // drops after a, whitespace and ccc
        )

        val input3 = toInput("aaa  ccc")
        // this shows behavior of whitespaceApi which requires quite tricky dropBuffer calls
        // it totally ignores first ~ and produces error in the second ~~
        assert(ab.parseInput(input3).isInstanceOf[Parsed.Failure])
      }

      'zeroDrops {
        import fastparse.all._

        val p = P( (("big string, " ~ ("another string, " ~ ("a".? ~/ "b".?)) | "small string, ") ~ "end of input") | "some other input" )

        val input = toInput("big string, another string, end of input")
        val Parsed.Success(_, i) = p.parseInput(input)
        assert(
          i == 40,
          Set(28, 40) == input.drops // drops after "another string" and "end of input"
        )
        //TODO it's a quite unexpected behavior
      }
    }
    'traceFailure{
      import fastparse.all._
      P("[" ~ "]").parse("[ ]").asInstanceOf[Parsed.Failure].extra.traced
      val e = intercept[RuntimeException] {
        P("[" ~ "]").parseIterator(Iterator("[", " ", "]")).asInstanceOf[Parsed.Failure].extra.traced
      }
      assert(e.getMessage.contains("Cannot perform `.traced` on an `IteratorParserInput`"))
    }
  }
}
