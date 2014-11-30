package scalaParser
package syntax
import acyclic.file
import org.parboiled2._
import macros.Macros._
trait Identifiers { self: Parser with Basic =>
  object Identifiers{
    import Basic._
    def Operator = rule{!Keywords ~ rep1(OpChar)}

    def VarId = VarId0(true)
    def VarId0(dollar: Boolean) = rule( !Keywords ~ Lower ~ IdRest(dollar) )
    def PlainId = rule( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator )
    def PlainIdNoDollar = rule( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
    def Id = rule( !Keywords ~ PlainId | ("`" ~ rep1(noneOf("`")) ~ "`") )
    def SkipChar(dollar: Boolean) = if(dollar) rule("_") else rule(anyOf("_$"))
    def IdRest(dollar: Boolean) = rule(
      rep(rep("_") ~ rep1(!SkipChar(dollar) ~ Letter | Digit)) ~ opt(rep1("_") ~ rep(OpChar))
    )

    def AlphabetKeywords = rule {
      (
        "abstract" | "case" | "catch" | "class" | "def" | "do" | "else" | "extends" | "false" | "finally" | "final" | "finally" | "forSome" | "for" | "if" |
        "implicit" | "import" | "lazy" | "match" | "new" | "null" | "object" | "override" | "package" | "private" | "protected" | "return" |
        "sealed" | "super" | "this" | "throw" | "trait" | "try" | "true" | "type" | "val" | "var" | "while" | "with" | "yield" | "_"
      ) ~ !Letter
    }
    def SymbolicKeywords = rule{
      (
        ":" | ";" | "=>" | "=" | "<-" | "<:" | "<%" | ">:" | "#" | "@" | "\u21d2" | "\u2190"
      )  ~ !OpChar
    }
    def Keywords = rule( AlphabetKeywords | SymbolicKeywords )
  }
}
