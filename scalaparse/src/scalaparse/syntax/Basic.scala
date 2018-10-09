package scalaparse.syntax


import fasterparser.Parsing._
import fasterparser._

object Basic {

  def UnicodeEscape[_: P] = P( "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits
  import fasterparser.NoWhitespace._
  val digits = "0123456789".toSet
  def Digit[_: P] = P( CharPred(digits) )
  val hexDigits = digits ++ "abcdefABCDEF"
  def HexDigit[_: P] = P( CharPred(hexDigits) )
  def HexNum[_: P] = P( "0x" ~ CharsWhile(hexDigits) )
  def DecNum[_: P] = P( CharsWhile(digits) )
  def Exp[_: P] = P( ("E" | "e") ~ ("+" | "-").? ~ DecNum )
  def FloatType[_: P] = P( "f" | "f" | "d" | "D" )

  def WSChars[_: P] = P( CharsWhile(c => c == '\u0020' || c == '\u0009') )
  def Newline[_: P] = P( StringIn("\r\n", "\n") )
  def Semi[_: P] = P( ";" | Newline.rep(1) )
  def OpChar[_: P] = P ( CharPred(isOpChar) )

  def isOpChar(c: Char) = c match{
    case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' |
         ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' => true
    case _ => isOtherSymbol(c) || isMathSymbol(c)
  }
  def Letter[_: P] = P( CharPred(c => isLetter(c) | isDigit(c) | c == '$' | c == '_' ) )
  val LetterDigitDollarUnderscore =  P(
    CharPred(c => isLetter(c) | isDigit(c) | c == '$' | c == '_' )
  )
  def Lower[_: P] = P( CharPred(c => isLower(c) || c == '$' | c == '_') )
  def Upper[_: P] = P( CharPred(isUpper) )
}
/**
 * Most keywords don't just require the correct characters to match,
 * they have to ensure that subsequent characters *don't* match in
 * order for it to be a keyword. This enforces that rule for key-words
 * (W) and key-operators (O) which have different non-match criteria.
 */
object Key {
  import fasterparser.NoWhitespace._
  def W[_: P](s: String) = P( s ~ !Basic.LetterDigitDollarUnderscore )(sourcecode.Name(s"`$s`"))
  // If the operator is followed by a comment, stop early so we can parse the comment
  def O[_: P](s: String) = P( s ~ (!Basic.OpChar | &("/*" | "//")) )(sourcecode.Name(s"`$s`"))
}
