package fastparse.iterator

import fastparse.all._
import utest._

object IteratorTests extends TestSuite {

  def toIterator(string: IndexedSeq[Char]) = string.map(IndexedSeq(_)).toIterator

  val tests = TestSuite {
    'cuts {
      'capturing {
        val p = P( "a" ~/ "b" ~/ "c")
        val capt = P( p.! ~ p.! ~ p.!)
        val Parsed.Success(res, i) = capt.parseIterator(toIterator("abcabcabc"))
        assert(
          i == 9,
          res == ("abc", "abc", "abc"))
      }

      'nocut {
        val p = P( "a" ~/ "b" ~/ "c")
        val nocut = P((NoCut(p) ~ NoCut(p) ~ NoCut(p)) | "abcd")
        val Parsed.Success(_, i1) = nocut.parseIterator(toIterator("abcabcabc"))
        assert(i1 == 9)
        val Parsed.Success(_, i2) = nocut.parseIterator(toIterator("abcd"))
        assert(i2 == 4)
      }

      'either {
        val p = P( "a" ~ "b" ~ "c")
        val either = P( (p ~ End) | ("abc" ~ p ~ End) | ("abcabc" ~ p ~ End))
        val Parsed.Success(_, i1) = either.parseIterator(toIterator("abcabcabc"))
        assert(i1 == 9)
        val Parsed.Success(_, i2) = either.parseIterator(toIterator("abcabc"))
        assert(i2 == 6)
        val Parsed.Success(_, i3) = either.parseIterator(toIterator("abc"))
        assert(i3 == 3)
      }

      'rep {
        val p = P( "a" ~ "b" ~ "c")
        val rep = P( (p.rep ~ "d") | (p.rep ~ "e") )
        val Parsed.Success(_, i1) = rep.parseIterator(toIterator("abcabcabcd"))
        assert(i1 == 10)
        val Parsed.Success(_, i2) = rep.parseIterator(toIterator("abcabcabce"))
        assert(i1 == 10)
      }

      'all {
        val p = P( "a" ~ "b" ~ "c" ~/ "d")
        val np = NoCut(p)
        val pp = P( "a" ~ "b" ~ "c" ~ End)

        val all = P( pp | (np ~/ np) | p ~ "e" | "abded".! )

        val Parsed.Success(res, _) = all.parseIterator(toIterator("abded"))
        assert(res == "abded")
      }
    }
  }
}
