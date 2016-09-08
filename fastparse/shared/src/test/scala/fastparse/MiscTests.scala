package fastparse
import all._
import utest._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MiscTests extends TestSuite{

  val tests = TestSuite{
    'toString{
      def check(p: Parser[_], s: String) = {
        assert(p.toString == s.trim)
      }
      'Either {
        check("A" | "B", """ "A" | "B" """)
        check("A" | "B" | "C", """ "A" | "B" | "C" """)
        check(("A" | "B") | "C", """ "A" | "B" | "C" """)
        check("A" | ("B" | "C"), """ "A" | "B" | "C" """)
      }
      'Sequence {
        check("A" ~ "BBB", """ "A" ~ "BBB" """)
        check("A" ~ "B" ~ "C", """ "A" ~ "B" ~ "C" """)
        check(("A" ~ "B") ~ "C", """ "A" ~ "B" ~ "C" """)
        // Not that this prints differently from the others; we
        // only collapse Sequence nodes on the left, and sequence
        // nodes on the right are harder to extract because of the
        // way each node's `ev` is called
        check("A" ~ ("B" ~ "C"), """ "A" ~ "B" ~ "C" """)
      }
      'Mixed{
        check(("A" ~ "B") | "C", """ "A" ~ "B" | "C" """)
        check("A" ~ ("B" | "C"), """ "A" ~ ("B" | "C")""")
        check(("A" | "B") ~ "C", """("A" | "B") ~ "C" """)
        check("A" | ("B" ~ "C"), """ "A" | "B" ~ "C" """)
      }
      'rep{
        check("A".rep, """ "A".rep """)
        check(("A" | "B").rep, """ ("A" | "B").rep """)
        check(("A".? | "B").rep, """ ("A".? | "B").rep """)
        check(("A".? | "B").rep(1), """ ("A".? | "B").rep(1) """)
        check(("A".? | "B").rep(1, max = 2), """ ("A".? | "B").rep(1, max = 2) """)
        check(("A".? | "B").rep(sep = "C"), """ ("A".? | "B").rep(sep = "C") """)
        check(("A".? | "B").rep(sep = "C", max = 2), """ ("A".? | "B").rep(sep = "C", max = 2) """)
        check(("A".? | "B").rep(1, sep="C" ~ "D" | "E"), """("A".? | "B").rep(1, sep = "C" ~ "D" | "E")""")
      }
      'lookahead{
        check(&("A") ~ "ABC", """&("A") ~ "ABC" """)
        check(!"A" ~ "ABC", """!("A") ~ "ABC" """)
        check("A".! ~ "ABC".!, """ "A" ~ "ABC" """)
      }
      'named{
        val Foo = P( "A" )
        check(Foo, """Foo""")
        check(End, """End""")
        check(Start, """Start""")
        check(Pass, """Pass""")
        check(Fail, """Fail""")
        check(AnyChar, """AnyChar""")
        check(CharIn("abc", "d", Seq('1', '2', '3')), """CharIn("abcd123")""")
        check(
          StringIn("mango", "mandarin", "mangosteen"),
          """StringIn("mango", "mandarin", "mangosteen")"""
        )
        check(CharPred(_.isUpper), """CharPred(<function1>)""")
      }
    }
    'logging{
      val logged = mutable.Buffer.empty[String]
      implicit val logger = fastparse.Logger(logged.append(_))

      val DeepFailure = P( "C" )
      val Foo = P( (DeepFailure.log() | "A".log()) ~ "B".!.log() ).log()

      Foo.parse("AB")

      val allLogged = logged.mkString("\n")

      val expected =
        """+Foo:0
          |  +DeepFailure:0
          |  -DeepFailure:0:Failure(DeepFailure:1:1 / "C":1:1 ..."AB")
          |  +"A":0
          |  -"A":0:Success(1)
          |  +"B":1
          |  -"B":1:Success(2)
          |-Foo:0:Success(2)
          |
        """.stripMargin.trim
      assert(allLogged == expected)
    }

    'flattening{
      'either{
        val E = parsers.Combinators.Either
        // Need to be pulled out because it makes utest crash
        val expected = E("A", "B", "C", "D")
        assert(("A" | "B" | "C" | "D") == expected)
        assert((("A" | "B") | ("C" | "D")) == expected)
        assert(("A" | ("B" | ("C" | "D"))) == expected)
      }
      'sequence{
        val S = parsers.Combinators.Sequence
        val F = S.Flat
        def C(p: P0, b: Boolean = false) = S.Chain(p, b)(null)
        // Need to be pulled out because it makes utest crash
        val expected1 = F("A", ArrayBuffer(C("B"), C("C"), C("D")))
        val expected2 = F("A", ArrayBuffer(C("B"), C(F("C", ArrayBuffer(C("D"))))))
        assert(
          ("A" ~ "B" ~ "C" ~ "D") == expected1,
          (("A" ~ "B") ~ ("C" ~ "D")) == expected2
        )
      }
    }
    'opaque{
      def checkOpaqueness[T](p: Parser[T], strs: String*) = strs foreach { str =>
        val failure = p.parse(str).asInstanceOf[Parsed.Failure]
        assert(failure.index == 0)
        assert(failure.extra.traced.traceParsers == Set(p))
      }
      'nocut{
        val p = P("foo" ~ CharPred(_.isDigit).rep(1)).opaque("fooX")
        checkOpaqueness(p, "fo", "fooz")
      }
      'cut{
        val p = P("foo" ~/ CharPred(_.isDigit).rep(1)).opaque("fooX")
        checkOpaqueness(p, "fo", "fooz")
      }
    }
    'wspStr{
      val literal = wspStr("ab")
      val charLiteral = wspStr("a")
      assert(
        literal.isInstanceOf[parsers.Terminals.Literal[Char, String]],
        charLiteral.isInstanceOf[parsers.Terminals.ElemLiteral[Char, String]]
      )
    }
    'failureget{
      val p = "A"
      intercept[ParseError]{
        p.parse("B").get
      }

    }
    'formatParser{
      assert(
        Parsed.Failure.formatParser("a", IndexedParserInput(""), 0) == """"a":0:0""",
        Parsed.Failure.formatParser("A", IndexedParserInput("B"), 0) == """"A":1:1""")
    }
    'utils{
      'trieNode {
        val names = (0 until 1000).map(_.toString.flatMap(_.toString * 5).toIndexedSeq)
        val trie = new Utils.TrieNode[Char](names)
        for (name <- names)
          assert(trie.query(IndexedParserInput(name), 0) != -1)
      }
    }
  }
}