package scalaParser
package syntax
import acyclic.file

import parsing._

object Basic {
  val UnicodeEscape = R( "\\u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits
  val HexDigit = R( CharIn("0123456789abcdefABCDEF") )
  val Digit = R( CharIn("0123456789") )
  val HexNum = R( "0x" ~ HexDigit.rep1 )
  val DecNum = R(Digit.rep1)
  val Exp = R( CharIn("Ee") ~ CharIn("+-").? ~ Digit.rep1 )
  val FloatType = R( CharIn("fFdD") )

  val WSChar = R( CharIn("\u0020\u0009") )(enclosingFunctionName)
  val Newline = R( StringIn("\r\n", "\n") )
  val Semi = R( ";" | Newline.rep1 )
  val OpChar = {
    // scalac 2.10 crashes if OtherOrMathSymbol below is substituted by its body
    // Same thing for LetterDigit, LowerChar, UpperChar
    val OtherOrMathSymbol = CharPred(_.getType match {
      case Character.OTHER_SYMBOL | Character.MATH_SYMBOL => true; case _ => false
    })

    R { CharIn("!#%&*+-/:<=>?@\\^|~") | OtherOrMathSymbol }
  }
  val Letter = {
    val LetterDigit = CharPred(c => c.isLetter | c.isDigit)
    R(Upper | Lower | LetterDigit )
  }
  val Lower = {
    val LowerChar = CharPred(_.isLower)
    R(CharIn("abcdefghijklmnopqrstuvwxyz$_") | LowerChar )
  }
  val Upper = {
    val UpperChar = CharPred(_.isUpper)
    R(CharIn("ABCDEFGHIJKLMNOPQRSTUCWXYZ") | UpperChar )
  }
}
/**
 * Most keywords don't just require the correct characters to match,
 * they have to ensure that subsequent characters *don't* match in
 * order for it to be a keyword. This enforces that rule for key-words
 * (W) and key-operators (O) which have different non-match criteria.
 */
object Key {
  def W(s: String) = R( s ~ !(Basic.Letter | Basic.Digit) )
  def O(s: String) = R( s ~ !Basic.OpChar )
}
