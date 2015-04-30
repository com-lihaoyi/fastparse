package parsing

import parsing.Parser.{Rule, StringIn}
import utest._

import scala.collection.mutable

object MiscTests extends TestSuite{

  val tests = TestSuite{
    'toString{
      def check(p: Parser[_], s: String) = {
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
        check(("A" ~ "B") | "C", """(("A" ~ "B") | "C")""")
        check("A" ~ ("B" | "C"), """("A" ~ ("B" | "C"))""")
        check(("A" | "B") ~ "C", """(("A" | "B") ~ "C")""")
        check("A" | ("B" ~ "C"), """("A" | ("B" ~ "C"))""")
      }
      'rep{
        check("A".rep, """ "A".rep """)
        check(("A" | "B").rep, """ ("A" | "B").rep """)
        check(("A".? | "B").rep, """ ("A".? | "B").rep """)
        check(("A".? | "B").rep1, """ ("A".? | "B").rep1 """)
        check(("A".? | "B").rep("C"), """ ("A".? | "B").rep("C") """)
        check(("A".? | "B").rep1("C" ~ "D" | "E"), """ ("A".? | "B").rep1((("C" ~ "D") | "E")) """)
      }
      'lookahead{
        check(&("A") ~ "ABC", """(&("A") ~ "ABC")""")
        check(!"A" ~ "ABC", """(!("A") ~ "ABC")""")
        check("A".! ~ "ABC".!, """("A".! ~ "ABC".!)""")
      }
      'named{
        val Foo = R( "A" )
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
      val Foo = R( "A".log("A", logged +=) ~ "B".!.log("B", logged +=) ).log("AB", logged+=)
      Foo.parse("AB")
      val expected = Seq(
        "+AB:0",
        "  +A:0",
        "  -A:0:Success((),1,false)",
        "  +B:1",
        "  -B:1:Success(B,2,false)",
        "-AB:0:Success(B,2,false)"
      )
      assert(logged == expected)
    }
    'flattening{
      'either{
        val E = Parser.Either
        assert(("A" | "B" | "C" | "D") == E("A", "B", "C", "D"))
        assert((("A" | "B") | ("C" | "D")) == E("A", "B", "C", "D"))
        assert(("A" | ("B" | ("C" | "D"))) == E("A", "B", "C", "D"))
      }
      'sequence{
        val S = Parser.Sequence
        val F = Parser.Sequence.Flat
        def C(p: R0, b: Boolean = false) = Parser.Sequence.Chain(p, b)(null)
        assert(
          ("A" ~ "B" ~ "C" ~ "D") == F("A", Vector(C("B"), C("C"), C("D"))),
          (("A" ~ "B") ~ ("C" ~ "D")) == F("A", Vector(C("B"), C(F("C", Vector(C("D"))))))
        )
      }
      'rule{""+{
        lazy val X: R0 = R( "A" | X )


        assert(rec(X, Nil) == ("A"| X))
        println(JsonTests.jsonExpr)
        println(rec(JsonTests.jsonExpr, Nil))
      }}
    }
  }
  def rec[T](p: Parser[T], stack: List[Parser[_]]): Parser[T] = {
    def recStack[T](pRec: Parser[T]): Parser[T] = rec(pRec, p :: stack)
    val res: Parser[T] = p match{
      case r: Rule[T] =>
        if (stack.contains(r)) r
        else recStack(r.pCached)
      case m: Parser.Mapper[_, T] => m.copy(p = recStack(m.p))
      case e: Parser.Either[T] => Parser.Either(e.ps.map(rec(_, p :: stack)):_*)
      case s: Parser.Sequence[_, _, T] => s.copy(p1 = recStack(s.p1), p2 = recStack(s.p2)).asInstanceOf[Parser[T]]
      case f: Parser.Sequence.Flat[T] => Parser.Sequence.Flat(
        recStack(f.p0),
        f.ps.map(c => Parser.Sequence.Chain(recStack(c.p), c.cut)(c.ev))
      )
      case p => p
    }
    res
  }
}
