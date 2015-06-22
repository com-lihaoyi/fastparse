package fastparse

import utest._

import scala.collection.mutable

object MiscTests extends TestSuite{

  val tests = TestSuite{
    'toString{
      def check(p: fastparse.core.Parser[_], s: String) = {
        assert(p.toString == s.trim)
      }
      'Either {
        check("A" | "B", """("A" | "B")""")
        check("A" | "B" | "C", """("A" | "B" | "C")""")
        check(("A" | "B") | "C", """("A" | "B" | "C")""")
        check("A" | ("B" | "C"), """("A" | "B" | "C")""")
      }
      'Sequence {
        check("A" ~ "BBB", """("A" ~ "BBB")""")
        check("A" ~ "B" ~ "C", """("A" ~ "B" ~ "C")""")
        check(("A" ~ "B") ~ "C", """("A" ~ "B" ~ "C")""")
        // Not that this prints differently from the others; we
        // only collapse Sequence nodes on the left, and sequence
        // nodes on the right are harder to extract because of the
        // way each node's `ev` is called
        check("A" ~ ("B" ~ "C"), """("A" ~ ("B" ~ "C"))""")
      }
      'Mixed{
        check(("A" ~ "B") | "C", """("A" ~ "B" | "C")""")
        check("A" ~ ("B" | "C"), """("A" ~ ("B" | "C"))""")
        check(("A" | "B") ~ "C", """(("A" | "B") ~ "C")""")
        check("A" | ("B" ~ "C"), """("A" | "B" ~ "C")""")
      }
      'rep{
        check("A".rep, """ "A".rep """)
        check(("A" | "B").rep, """ ("A" | "B").rep """)
        check(("A".? | "B").rep, """ ("A".? | "B").rep """)
        check(("A".? | "B").rep(1), """ ("A".? | "B").rep(1) """)
        check(("A".? | "B").rep(sep = "C"), """ ("A".? | "B").rep(sep = "C") """)
        check(("A".? | "B").rep(1, sep="C" ~ "D" | "E"), """ ("A".? | "B").rep(1, sep = ("C" ~ "D" | "E")) """)
      }
      'lookahead{
        check(&("A") ~ "ABC", """(&("A") ~ "ABC")""")
        check(!"A" ~ "ABC", """(!("A") ~ "ABC")""")
        check("A".! ~ "ABC".!, """("A" ~ "ABC")""")
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
      val Foo = P( "A".log("A") ~ "B".!.log("B") ).log("AB")
      Foo.parse("AB")
      def expected(unit: String) = mutable.Buffer(
        "+AB:0",
        "  +A:0",
        s"  -A:0:Success($unit, 1)",
        "  +B:1",
        "  -B:1:Success(B, 2)",
        "-AB:0:Success(B, 2)"
      )
      val expected1 = expected("()")
      val expected2 = expected("undefined")
      assert(logged == expected1 || logged == expected2)
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
        val F = parsers.Combinators.Sequence.Flat
        def C(p: P0, b: Boolean = false) = parsers.Combinators.Sequence.Chain(p, b)(null)
        // Need to be pulled out because it makes utest crash
        val expected1 = F("A", Vector(C("B"), C("C"), C("D")))
        val expected2 = F("A", Vector(C("B"), C(F("C", Vector(C("D"))))))
        assert(
          ("A" ~ "B" ~ "C" ~ "D") == expected1,
          (("A" ~ "B") ~ ("C" ~ "D")) == expected2
        )
      }
    }
    'wspStr{
      val literal = wspStr("ab")
      val charLiteral = wspStr("a")
      assert(
        literal.isInstanceOf[parsers.Terminals.Literal],
        charLiteral.isInstanceOf[parsers.Terminals.CharLiteral]
      )
    }
  }
}
