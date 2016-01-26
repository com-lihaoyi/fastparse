package scalaparse.syntax

import acyclic.file

import fastparse.all._
import fastparse.CharPredicates._
object Basic {
  val UnicodeEscape = P( "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits

  val digits = "0123456789"
  val Digit = P( CharIn(digits) )
  val hexDigits = digits + "abcdefABCDEF"
  val HexDigit = P( CharIn(hexDigits) )
  val HexNum = P( "0x" ~ CharsWhile(hexDigits.contains(_)) )
  val DecNum = P( CharsWhile(digits.contains(_)) )
  val Exp = P( CharIn("Ee") ~ CharIn("+-").? ~ DecNum )
  val FloatType = P( CharIn("fFdD") )

  val WSChars = P( CharsWhile("\u0020\u0009".contains(_)) )
  val Newline = P( StringIn("\r\n", "\n") )
  val Semi = P( ";" | Newline.rep(1) )
  val OpChar = P ( CharPred(isOpChar) )

  def isOpChar(c: Char) = {
    // scalac 2.10 crashes if OtherOrMathSymbol below is substituted by its body
    // Same thing for LetterDigit, LowerChar, UpperChar
    fastparse.CharPredicates.isOtherSymbol(c) || fastparse.CharPredicates.isMathSymbol(c) || "!#%&*+-/:<=>?@\\^|~".contains(c)
  }
  val Letter = P( CharPred(c => c.isLetter | c.isDigit | "$_".contains(c)) )
  val LetterDigitDollarUnderscore =  P( CharPred(c => c.isLetter | c.isDigit | "$_".contains(c) ) )
  val Lower = P( CharPred(c => c.isLower || "$_".contains(c)) )
  val Upper = P( CharPred(_.isUpper) )
}
/**
 * Most keywords don't just require the correct characters to match,
 * they have to ensure that subsequent characters *don't* match in
 * order for it to be a keyword. This enforces that rule for key-words
 * (W) and key-operators (O) which have different non-match criteria.
 */
object Key {
  def W(s: String) = P( s ~ !Basic.LetterDigitDollarUnderscore )(sourcecode.Name(s"`$s`"))
  // If the operator is followed by a comment, stop early so we can parse the comment
  def O(s: String) = P( s ~ (!Basic.OpChar | &("/*" | "//")) )(sourcecode.Name(s"`$s`"))
}
