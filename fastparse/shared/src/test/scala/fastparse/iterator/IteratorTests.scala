package fastparse.iterator

import fastparse.{ElemTypeFormatter, IteratorParserInput}
import fastparse.all._
import utest._

import scala.collection.mutable

object IteratorTests extends TestSuite {

  class LoggedDropsParserInput[ElemType](data: Iterator[IndexedSeq[ElemType]])
                                        (implicit formatter: ElemTypeFormatter[ElemType])
    extends IteratorParserInput[ElemType](data) {

    var drops = mutable.Set.empty[Int]

    override def dropBuffer(index: Int): Unit = {
      drops.add(index)
      super.dropBuffer(index)
    }
  }

  def toInput(string: String) = new LoggedDropsParserInput[Char](string.grouped(1).map(wrapString))

  val tests = TestSuite {
    'cuts {
      'capturing {
        val p = P( "a" ~/ "b" ~/ "c")
        val capt = P( p.! ~ p.! ~ p.!)
        val input = toInput("abcabcabc")
        val Parsed.Success(res, i) = capt.parseInput(input)
        assert(
          i == 9,
          res == ("abc", "abc", "abc"),
          Seq(3, 6, 9).forall(input.drops.contains)
        )
      }

      'nocut {
        val p = P( "a" ~/ "b" ~/ "c")
        val nocut = P((NoCut(p) ~ NoCut(p) ~/ NoCut(p)) | "abcd")

        val input1 = toInput("abcabcabc")
        val Parsed.Success(_, i1) = nocut.parseInput(input1)
        assert(
          i1 == 9,
          Seq(9).forall(input1.drops.contains)
        )

        val input2 = toInput("abcd")
        val Parsed.Success(_, i2) = nocut.parseInput(input2)
        assert(i2 == 4)
      }

      'either {
        val p = P( "a" ~ "b" ~ "c")
        val either = P( (p ~ End) | ("abc" ~ p ~ End) | ("abcabc" ~ p ~ End))

        val input1 = toInput("abcabcabc")
        val Parsed.Success(_, i1) = either.parseInput(input1)
        assert(
          i1 == 9,
          Seq(6, 7, 8, 9).forall(input1.drops.contains)
        )

        val input2 = toInput("abcabc")
        val Parsed.Success(_, i2) = either.parseInput(input2)
        assert(i2 == 6)

        val input3 = toInput("abc")
        val Parsed.Success(_, i3) = either.parseInput(input3)
        assert(i3 == 3)
      }

      'rep {
        val p = P( "a" ~ "b" ~ "c")
        val rep = P( (p.rep ~ "d") | (p.rep ~ "e") )

        val input1 = toInput("abcabcabcd")
        val Parsed.Success(_, i1) = rep.parseInput(input1)
        assert(i1 == 10)

        val input2 = toInput("abcabcabce")
        val Parsed.Success(_, i2) = rep.parseInput(input2)
        assert(
          i1 == 10,
          Seq(9, 10).forall(input2.drops.contains)
        )
      }

      'all {
        val p = P( "a" ~ "b" ~ "c" ~/ "d")
        val np = NoCut(p)
        val pp = P( "a" ~ "b" ~ "c" ~ End)

        val all = P( pp | (np ~/ np) | p ~ "e" | "abded".! )

        val input = toInput("abded")

        val Parsed.Success(res, _) = all.parseInput(input)
        assert(res == "abded")
      }
    }
  }
}
