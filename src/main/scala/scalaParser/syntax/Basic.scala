package scalaParser
package syntax
import acyclic.file
import org.parboiled2._

trait Basic { self: Parser =>
  object Basic{
    def UnicodeExcape = rule( "\\u" ~ 4.times(HexDigit) )

    //Numbers and digits
    def HexDigit = rule( Digit | "a" - "f" | "A" - "Z" )
    def Digit = rule( "0" | NonZeroDigit )
    def NonZeroDigit = rule( "1" - "9" )
    def HexNumeral = rule( "0x" ~ oneOrMore(HexDigit) )
    def DecimalNumeral = rule(oneOrMore(Digit))
    def ExponentPart = rule( anyOf("Ee") ~ optional(anyOf("+-")) ~ oneOrMore(Digit) )
    def FloatType = rule( anyOf("FfDd") )

    def Parentheses = rule( "(" | ")" | "[" | "]" | "{" | "}" )
    def DelimiterChar = rule( "'" | "\"" | "." | ";" | "," )

    def WhitespaceChar = rule( "\u0020" | "\u0009" )
    def Newline = rule( "\r\n" | "\n" )
    def Semi = rule( ';' | oneOrMore(Newline) )
    def OperatorChar = rule {
      anyOf("""!#$%&*+-/:<=>?@\^|~""") |
      CharPredicate.from(_.getType match {
        case Character.OTHER_SYMBOL | Character.MATH_SYMBOL => true; case _ => false
      })
    }
    def Letter = rule( Upper | Lower | CharPredicate.from(c => c.isLetter | c.isDigit) )
    def Lower = rule( "a" - "z" | "$" | "_" | CharPredicate.from(_.isLower) )
    def Upper = rule( "A" - "Z" | CharPredicate.from(_.isUpper) )
  }
  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object Key {
    def W(s: String) = rule( str(s) ~ !(Basic.Letter | Basic.Digit) )

    def O(s: String) = rule( str(s) ~ !Basic.OperatorChar )
  }
}
