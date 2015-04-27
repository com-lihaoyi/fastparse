package scalaParser
package syntax
import acyclic.file
import parsing.Parsing.Parser.CharPredicate
import parsing.Parsing._

object Basic {
  val UnicodeEscape = rule( "\\u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits
  val HexDigit = rule( CharPredicate("0123456789abcdefABCDEF".toSet) )
  val Digit = rule( CharPredicate("0123456789".toSet) )
  val HexNum = rule( "0x" ~ HexDigit.rep1 )
  val DecNum = rule(Digit.rep1)
  val Exp = rule( ("E"|"e") ~ ("+"|"-").? ~ Digit.rep1 )
  val FloatType = rule( CharPredicate("fFdD".toSet) )

  val WSChar = rule( "\u0020" | "\u0009" )
  val Newline = rule( "\r\n" | "\n" )
  val Semi = rule( ";" | Newline.rep1 )
  val OpChar = {
    // scalac 2.10 crashes if OtherOrMathSymbol below is substituted by its body
    // Same thing for LetterDigit, LowerChar, UpperChar
    val OtherOrMathSymbol = CharPredicate(_.getType match {
      case Character.OTHER_SYMBOL | Character.MATH_SYMBOL => true; case _ => false
    })

    rule { CharPredicate("[!#%&*+-/:<=>?@\\^|~]".toSet) | OtherOrMathSymbol }
  }
  val Letter = {
    val LetterDigit = CharPredicate(c => c.isLetter | c.isDigit)
    rule(Upper | Lower | LetterDigit )
  }
  val Lower = {
    val LowerChar = CharPredicate(_.isLower)
    rule(CharPredicate("abcdefghijklmnopqrstuvwxyz$_".toSet) | LowerChar )
  }
  val Upper = {
    val UpperChar = CharPredicate(_.isUpper)
    rule(CharPredicate("ABCDEFGHIJKLMNOPQRSTUCWXYZ".toSet) | UpperChar )
  }
}
/**
 * Most keywords don't just require the correct characters to match,
 * they have to ensure that subsequent characters *don't* match in
 * order for it to be a keyword. This enforces that rule for key-words
 * (W) and key-operators (O) which have different non-match criteria.
 */
object Key {
  def W(s: String) = rule( s ~ !(Basic.Letter | Basic.Digit) )
  def O(s: String) = rule( s ~ !Basic.OpChar )
}
