package scalaParser
package syntax
import acyclic.file
import org.parboiled2._

trait Basic { self: Parser =>
  object Basic{
    def UnicodeEscape = rule( "\\u" ~ 4.times(HexDigit) )

    //Numbers and digits
    def HexDigit = rule( Digit | "a" - "f" | "A" - "Z" )
    def Digit = rule( "0" - "9" )
    def HexNum = rule( "0x" ~ oneOrMore(HexDigit) )
    def DecNum = rule(oneOrMore(Digit))
    def Exp = rule( anyOf("Ee") ~ optional(anyOf("+-")) ~ oneOrMore(Digit) )
    def FloatType = rule( anyOf("FfDd") )

    def WSChar = rule( "\u0020" | "\u0009" )
    def Newline = rule( "\r\n" | "\n" )
    def Semi = rule( ";" | oneOrMore(Newline) )
    def OpChar = {
      // scalac 2.10 crashes if OtherOrMathSymbol below is substituted by its body
      // Same thing for LetterDigit, LowerChar, UpperChar
      def OtherOrMathSymbol = CharPredicate.from(_.getType match {
        case Character.OTHER_SYMBOL | Character.MATH_SYMBOL => true; case _ => false
      })

      rule { anyOf("""!#%&*+-/:<=>?@\^|~""") | OtherOrMathSymbol }
    }
    def Letter = {
      def LetterDigit = CharPredicate.from(c => c.isLetter | c.isDigit)
      rule(Upper | Lower | LetterDigit )
    }
    def Lower = {
      def LowerChar = CharPredicate.from(_.isLower)
      rule("a" - "z" | "$" | "_" | LowerChar )
    }
    def Upper = {
      def UpperChar = CharPredicate.from(_.isUpper)
      rule("A" - "Z" | UpperChar )
    }
  }
  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object Key {
    def W(s: String) = rule( str(s) ~ !(Basic.Letter | Basic.Digit) )
    def O(s: String) = rule( str(s) ~ !Basic.OpChar )
  }
}
