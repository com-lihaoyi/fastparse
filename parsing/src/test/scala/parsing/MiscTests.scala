package parsing

import parsing.Parser.CharTrie
import utest._

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
        check("A" ~ ("B" ~ "C"), """("A" ~ "B" ~ "C")""")
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
        def Foo = R( "A" )
        check(Foo, """Foo""")
        check(End, """End""")
        check(Start, """Start""")
        check(Pass, """Pass""")
        check(Fail, """Fail""")
        check(AnyChar, """AnyChar""")
        check(CharSets("abc", "d", Seq('1', '2', '3')), """CharSets("abcd123")""")
        check(
          CharTrie("mango", "mandarin", "mangosteen"),
          """CharTrie("mango", "mandarin", "mangosteen")"""
        )
        check(CharPred(_.isUpper), """CharPred(<function1>)""")
      }
    }
  }
}
