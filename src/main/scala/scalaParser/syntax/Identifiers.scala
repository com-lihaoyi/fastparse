package scalaParser
package syntax
import acyclic.file
import org.parboiled2._

trait Identifiers { self: Parser with Basic =>
  object Identifiers{
    import Basic._
    def Operator = rule{!Keywords ~ OpChar.+}

    def VarId = VarId0(true)
    def VarId0(dollar: Boolean) = rule( !Keywords ~ Lower ~ IdRest(dollar) )
    def PlainId = rule( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator )
    def PlainIdNoDollar = rule( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
    def Id = rule( !Keywords ~ PlainId | ("`" ~ noneOf("`").+ ~ "`") )

    def IdRest(allowDollar: Boolean) = {
      def SkipChar = if(allowDollar) rule("_") else rule(anyOf("_$"))
      def IdUnderscoreChunk = rule( zeroOrMore("_") ~ (!SkipChar ~ Letter | Digit).+ )
      rule( IdUnderscoreChunk.* ~ (oneOrMore("_") ~ OpChar.*).? )
    }

    def AlphabetKeywords = rule {
      (
        "abstract" | "case" | "catch" | "class" | "def" | "do" | "else" |
        "extends" | "false" | "finally" | "final" | "finally" | "forSome" |
        "for" | "if" | "implicit" | "import" | "lazy" | "match" | "new" |
        "null" | "object" | "override" | "package" | "private" | "protected" |
        "return" | "sealed" | "super" | "this" | "throw" | "trait" | "try" |
        "true" | "type" | "val" | "var" | "while" | "with" | "yield" | "_"
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
