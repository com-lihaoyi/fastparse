package scalaParser
package syntax
import acyclic.file
import parsing.Parsing.Parser.CharPred
import parsing.Parsing._

object Basic {
  val UnicodeEscape = rule( "\\u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits
  val HexDigit = rule( Parser.CharIn("0123456789abcdefABCDEF") )
  val Digit = rule( Parser.CharIn("0123456789") )
  val HexNum = rule( "0x" ~ HexDigit.rep1 )
  val DecNum = rule(Digit.rep1)
  val Exp = rule( ("E"|"e") ~ ("+"|"-").? ~ Digit.rep1 )
  val FloatType = rule( Parser.CharIn("fFdD") )

  val WSChar = rule( "\u0020" | "\u0009" )(enclosingFunctionName)
  val Newline = rule( "\r\n" | "\n" )
  val Semi = rule( ";" | Newline.rep1 )
  val OpChar = {
    // scalac 2.10 crashes if OtherOrMathSymbol below is substituted by its body
    // Same thing for LetterDigit, LowerChar, UpperChar
    val OtherOrMathSymbol = CharPred(_.getType match {
      case Character.OTHER_SYMBOL | Character.MATH_SYMBOL => true; case _ => false
    })

    rule { Parser.CharIn("!#%&*+-/:<=>?@\\^|~") | OtherOrMathSymbol }
  }
  val Letter = {
    val LetterDigit = CharPred(c => c.isLetter | c.isDigit)
    rule(Upper | Lower | LetterDigit )
  }
  val Lower = {
    val LowerChar = CharPred(_.isLower)
    rule(Parser.CharIn("abcdefghijklmnopqrstuvwxyz$_") | LowerChar )
  }
  val Upper = {
    val UpperChar = CharPred(_.isUpper)
    rule(Parser.CharIn("ABCDEFGHIJKLMNOPQRSTUCWXYZ") | UpperChar )
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
