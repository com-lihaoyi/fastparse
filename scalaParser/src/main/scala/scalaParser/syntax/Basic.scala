package scalaParser
package syntax
import acyclic.file
import parsing.Parser.CharsWhile

import parsing._

object Basic {
  val UnicodeEscape = R( "\\u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits

  val hexDigits = "0123456789abcdefABCDEF"
  val HexDigit = R( CharIn(hexDigits) )
  val digits = "0123456789"
  val Digit = R( CharIn(digits) )
  val HexNum = R( "0x" ~ CharsWhile(hexDigits.contains(_), min = 1) )
  val DecNum = R( CharsWhile(digits.contains(_), min = 1) )
  val Exp = R( CharIn("Ee") ~ CharIn("+-").? ~ DecNum )
  val FloatType = R( CharIn("fFdD") )

  val WSChar = R( CharIn("\u0020\u0009") )(enclosingFunctionName)
  val Newline = R( StringIn("\r\n", "\n") )
  val Semi = R( ";" | Newline.rep1 )
  val OpChar = R ( CharPred(isOpChar) )

  def isOpChar(c: Char) = {
    // scalac 2.10 crashes if OtherOrMathSymbol below is substituted by its body
    // Same thing for LetterDigit, LowerChar, UpperChar
    c.getType == Character.OTHER_SYMBOL || c.getType == Character.MATH_SYMBOL || "!#%&*+-/:<=>?@\\^|~".contains(c)
  }
  val Letter = R( CharPred(c => c.isLetter | c.isDigit | "$_".contains(c)) )
  val LetterDigitDollarUnderscore =  R( CharPred(c => c.isLetterOrDigit || "$_".contains(c) ) )
  val Lower = R( CharPred(c => c.isLower || "$_".contains(c)) )
  val Upper = R( CharPred(_.isUpper) )
}
/**
 * Most keywords don't just require the correct characters to match,
 * they have to ensure that subsequent characters *don't* match in
 * order for it to be a keyword. This enforces that rule for key-words
 * (W) and key-operators (O) which have different non-match criteria.
 */
object Key {
  def W(s: String) = R( s ~ !Basic.LetterDigitDollarUnderscore )
  def O(s: String) = R( s ~ !Basic.OpChar )
}
