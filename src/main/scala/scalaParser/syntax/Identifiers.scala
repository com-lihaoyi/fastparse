package scalaParser
package syntax
import acyclic.file
import org.parboiled2._

trait Identifiers { self: Parser with Basic =>
  object Identifiers{
    import Basic._
    def Operator = rule{!Keywords ~ oneOrMore(OperatorChar)}

    def VarId = VarId0(true)
    def VarId0(dollar: Boolean) = rule { !Keywords ~ Lower ~ IdRest(dollar) }
    def PlainId = rule { !Keywords ~ Upper ~ IdRest(true) | VarId | Operator }
    def PlainIdNoDollar = rule { !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator }
    def Id = rule { !Keywords ~ PlainId | ("`" ~ oneOrMore(noneOf("`")) ~ "`") }
    def IdRest(dollar: Boolean) = {
      if (!dollar) rule {
        zeroOrMore(zeroOrMore("_") ~ oneOrMore(!anyOf("_$") ~ Letter | Digit)) ~
        optional(oneOrMore("_") ~ zeroOrMore(OperatorChar))
      } else rule{
        zeroOrMore(zeroOrMore("_") ~ oneOrMore(!"_" ~ Letter | Digit)) ~
        optional(oneOrMore("_") ~ zeroOrMore(OperatorChar))
      }
    }

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
      )  ~ !OperatorChar
    }
    def Keywords = rule {
      AlphabetKeywords | SymbolicKeywords

    }

  }
}
