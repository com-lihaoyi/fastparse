package scalaParser
package syntax
import acyclic.file
import org.parboiled2._

trait Literals { self: Parser with Basic with Identifiers =>
  def Block: Rule0
  def WL: Rule0
  object Literals{
    import Basic._
    def FloatingPointLiteral = rule {

      "." ~ oneOrMore(Digit) ~ optional(ExponentPart) ~ optional(FloatType) |
      oneOrMore(Digit) ~ (
        "." ~ oneOrMore(Digit) ~ optional(ExponentPart) ~ optional(FloatType) |
        ExponentPart ~ optional(FloatType) |
        optional(ExponentPart) ~ FloatType
      )
    }

    def IntegerLiteral = rule { (HexNumeral | DecimalNumeral) ~ optional(anyOf("Ll")) }

    def BooleanLiteral = rule { Key.W("true") | Key.W("false")  }

    def MultilineComment: Rule0 = rule { "/*" ~ zeroOrMore(MultilineComment | !"*/" ~ ANY) ~ "*/" }
    def Comment: Rule0 = rule {
      MultilineComment |
      "//" ~ zeroOrMore(!Basic.Newline ~ ANY) ~ &(Basic.Newline | EOI)
    }

    def Literal = rule {
      (optional("-") ~ (FloatingPointLiteral | IntegerLiteral)) |
      BooleanLiteral |
      CharacterLiteral |
      StringLiteral |
      SymbolLiteral |
      (Key.W("null") ~ !(Basic.Letter | Basic.Digit))
    }

    def EscapedChars = rule { '\\' ~ anyOf("0btnfr'\\\"") }

    // Note that symbols can take on the same values as keywords!
    def SymbolLiteral = rule { ''' ~ (Identifiers.PlainId | Identifiers.Keywords) }

    def CharacterLiteral = rule {
      ''' ~ (UnicodeExcape | EscapedChars | !'\\' ~ CharPredicate.from(isPrintableChar)) ~ '''
    }

    def MultiLineChars = rule {
      zeroOrMore(Interpolation | optional('"') ~ optional('"') ~ noneOf("\""))
    }
    def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }
    def Interpolation = rule{
      "$" ~ Identifiers.PlainIdNoDollar | "${" ~ Block ~ WL ~ "}" | "$$"
    }
    def StringLiteral = rule {
      (Identifiers.Id ~ "\"\"\"" ~ MultiLineChars ~ ("\"\"\"" ~ zeroOrMore('"'))) |
      (Identifiers.Id ~ '"' ~ zeroOrMore(Interpolation | "\\\"" | "\\\\" | noneOf("\n\"")) ~ '"') |
      ("\"\"\"" ~ MultiLineChars ~ ("\"\"\"" ~ zeroOrMore('"'))) |
      ('"' ~ zeroOrMore("\\\"" | "\\\\" | noneOf("\n\"")) ~ '"')
    }

    def isPrintableChar(c: Char): Boolean = {
      val block = Character.UnicodeBlock.of(c)
      !Character.isISOControl(c) && !Character.isSurrogate(c) && block != null && block != Character.UnicodeBlock.SPECIALS
    }
  }
}

