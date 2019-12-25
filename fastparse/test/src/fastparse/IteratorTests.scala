package test.fastparse
import fastparse._
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
    new LoggedDropsParserInput(string.grouped(1))
  }

  val tests = Tests {
    test("basic"){
      import NoWhitespace._
      def p[_: P] = P( "ab" ~/ "cd".rep().! ~ "ef" | "z" )

      val Parsed.Success(res, i) = parse(Iterator("ab", "cd", "cd", "cd", "ef"), p(_))

      assert(res == "cdcdcd")
    }

    test("readable"){
      for(bufferSize <- Range(1, 15)){
        import NoWhitespace._
        def p[_: P] = P("ab" ~/ "cd".rep().! ~ "ef" | "z")

        val Parsed.Success(res, i) = parse(
          ParserInputSource.FromReadable("abcdcdcdef", bufferSize),
          p(_)
        )

        assert(res == "cdcdcd")
      }
    }

    test("immediateCutDrop"){
      import NoWhitespace._
      def p[_: P] = P( "ab" ~/ "cd" | "z" ).log

      val input = toInput("abcdef")
      val Parsed.Success(res, i) = parse(input, p(_))
      // Make sure that we drop immediately at position 2, since that is where
      // the cut has taken place, rather than at position 4 as we did earlier.
      assert(input.drops == Set(2, 4))
    }

    test("whitespaceImmediateCutDrop"){
      import NoWhitespace._
      implicit def whitespace{implicit ctx: P[_] =>
        " ".? ~ " ".rep
      }

      def p[_: P] = P( "ab" ~/ "cd" | "z" )

      val input = toInput("abcdef")
      val Parsed.Success(res, i) = parse(input, p(_))
      // Make sure that we drop immediately at position 2, since that is where
      // the cut has taken place, rather than at position 4 as we did earlier.
      assert(input.drops == Set(2, 4))
    }

    test("topLevelNoCuts"){
      import NoWhitespace._
      // Top-level sequences, which are not inside any `|`s or `.rep`s or `.?`s,
      // should dropBuffer immediately after every `~`, even without any cuts

      def p[_: P] = P( "a" ~ "b" ~ "c")
      def capt[_ : P] = P( p ~ p ~ p)
      val input = toInput("abcabcabc")
      val Parsed.Success(res, i) = parse(input, capt(_))
      println(i)
      assert(input.drops == Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    test("cuts"){
      test("capturing"){
        import NoWhitespace._

        def p[_: P] = P( "a" ~/ "b" ~/ "c")
        def capt[_: P] = P( p.! ~ p.! ~ p.!)
        val input = toInput("abcabcabc")
        val Parsed.Success(res, i) = parse(input, capt(_))
        assert(
          i == 9,
          res == ("abc", "abc", "abc"),
          Set(3, 6, 9) == input.drops  // drops sequence elements
        )
      }

      test("nocut"){
        import NoWhitespace._

        def p[_: P] = P( "a" ~/ "b" ~/ "c")
        def nocut[_: P] = P((NoCut(p) ~ NoCut(p) ~/ NoCut(p)) | "abcd")

        val input1 = toInput("abcabcabc")
        val Parsed.Success(_, i1) = parse(input1, nocut(_))
        assert(
          i1 == 9,
          Set(6, 9) == input1.drops // drop non-droppable NoCut block after cut
        )

        val input2 = toInput("abcd")
        val Parsed.Success(_, i2) = parse(input2, nocut(_))
        assert(
          i2 == 4,
          input2.drops.isEmpty // no drops during simple parsers, for instance literal
        )
      }

      test("either"){
        import NoWhitespace._
        def p[_: P] = P( "a" ~ "b" ~ "c")
        def either[_: P] = P( (p ~ End) | ("abc" ~ p ~ End) | ("abcabc" ~ p ~ End))
        def eitherCutted[_: P] = P( (p ~ End) | ("abc" ~ p ~ End) | ("abcabc" ~/ p ~ End))

        val input1 = toInput("abcabcabc")
        val Parsed.Success(_, i1) = parse(input1, either(_))
        assert(
          i1 == 9,
          // We do not expect any drops in the last branch of the Either,
          // without explicit cuts. This is because need to backtrack the index
          // of the parser back to the Either's starting position anyway, and
          // if we drop input it might cause problems e.g. with rendering
          // the error message when the data around the failure index has
          // already been dropped
          Set() == input1.drops
        )
        val Parsed.Success(_, i2) = parse(input1, eitherCutted(_))
        assert(
          i2 == 9,
          // With explicit cuts, we can accept drops
          Set(6, 7, 8, 9) == input1.drops
        )

        val input2 = toInput("abcabc")
        val Parsed.Success(_, i3) = parse(input2, either(_))
        assert(
          i3 == 6,
          input2.drops.isEmpty // no drops at the end
        )

        val input3 = toInput("abc")
        val Parsed.Success(_, i4) = parse(input3, either(_))
        assert(
          i4 == 3,
          input3.drops.isEmpty // no drops at the end
        )
      }

      test("rep"){
        import NoWhitespace._
        def p[_: P] = P( "a" ~ "b" ~ "c")
        def rep[_: P] = P( (p.rep ~ "d") | (p.rep ~ "e") )
        def repCutted[_: P] = P( (p.rep ~ "d") | (p.rep ~/ "e") )

        val input1 = toInput("abcabcabcd")
        val Parsed.Success(_, i1) = parse(input1, rep(_))
        assert(
          i1 == 10,
          Set() == input1.drops // no drops at the end of first branch
        )

        val input2 = toInput("abcabcabce")
        val Parsed.Success(_, i2) = parse(input2, rep(_))

        assert(
          i1 == 10,
          Set() == input1.drops // no drops in the last branch, without cuts
        )

        val input3 = toInput("abcabcabce")
        val Parsed.Success(_, i3) = parse(input3, repCutted(_))

        assert(
          i1 == 10,
          Set(9, 10) == input3.drops // drops in the last branch, when cuts are present
        )
      }

      test("all"){
        import NoWhitespace._
        def p[_: P] = P( "a" ~ "b" ~ "c" ~/ "d")
        def np[_: P] = NoCut(p)
        def pp[_: P] = P( "a" ~ "b" ~ "c" ~ End)
        def all[_: P] = P( pp | (np ~/ np) | p ~ "e" | "abded".! )

        val input = toInput("abded")

        val Parsed.Success(res, _) = parse(input, all(_))
        assert(
          res == "abded",
          input.drops.isEmpty // no drops in literal
        )
      }

      test("whitespaceApi"){

        implicit def whitespace = { implicit ctx: P[_] =>
          " ".? ~~/ " ".repX
        }

        def a[_: P] = P( "aaa" )
        def b[_: P] = P( "bbb" )
        def ab[_: P] = P( a ~ b.? ~~ " " ~~ "ccc" )

        val input1 = toInput("aaa   bbb ccc")
        val Parsed.Success(_, i1) = parse(input1, ab(_))
        val drops = input1.drops
        assert(
          i1 == 13,
          Set(3, 9, 10, 13) == drops// drops after a, b and at the end
          // no drops within the whitespaces, in spite of cut
        )

        val input2 = toInput("aaa ccc")
        val Parsed.Success(_, i2) = parse(input2, ab(_))
        assert(
          i2 == 7,
          Set(3, 4, 7) == input2.drops // drops after a, whitespace and ccc
        )

        val input3 = toInput("aaa  ccc")
        // this shows behavior of whitespaceApi which requires quite tricky dropBuffer calls
        // it totally ignores first ~ and produces error in the second ~~
        assert(parse(input3, ab(_)).isInstanceOf[Parsed.Failure])
      }

      test("zeroDrops"){
        import NoWhitespace._
        def p[_: P] = P(
          (("big, " ~ ("another, " ~ ("X".? ~/ "Y".?)) | "small, ") ~ "end") | "other"
        )
        val input = toInput("big, another, end")
        val Parsed.Success(_, i) = parse(input, p(_))
        val drops = input.drops
        assert(
          i == 17,
          Set(14, 17) == drops
          // drops after "another, " because of the nested cut ~/, and again
          // after `"end" because the cut prevents it from backtracking out of
          // the most outer `|`
        )

      }
    }

    test("traceFailure"){
      import NoWhitespace._
      def p[_: P] = P("[" ~ "]")

      parse("[ ]", p(_)).asInstanceOf[Parsed.Failure].extra.traced
      val e = intercept[RuntimeException] {
        parse(Iterator("[", " ", "]"), p(_)).asInstanceOf[Parsed.Failure].extra.traced
      }
      assert(e.getMessage.contains("Cannot perform `.traced` on an `fastparse.IteratorParserInput`"))
    }
  }
}
