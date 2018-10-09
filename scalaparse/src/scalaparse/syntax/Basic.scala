package scalaparse.syntax


import fasterparser.Parsing._
import fasterparser._
import fasterparser.NoWhitespace._
import CharPredicates._
object Basic {

  def UnicodeEscape[_: P] = P( "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits
  def Digit[_: P] = P( CharIn("0-9") )

  def HexDigit[_: P] = P( CharIn("0-9a-fA-F") )
  def HexNum[_: P] = P( "0x" ~ CharsWhileIn("0-9a-fA-F") )
  def DecNum[_: P] = P( CharsWhileIn("0-9") )
  def Exp[_: P] = P( CharIn("Ee") ~ CharIn("+\\-").? ~ DecNum )
  def FloatType[_: P] = P( CharIn("fFdD") )

  def WSChars[_: P] = P( CharsWhileIn("\u0020\u0009") )
  def Newline[_: P] = P( StringIn("\r\n", "\n") )
  def Semi[_: P] = P( ";" | Newline.rep(1) )
  def OpChar[_: P] = P ( CharPred(isOpChar) )

  def isOpChar(c: Char) = c match{
    case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' |
         ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' => true
    case _ => isOtherSymbol(c) || isMathSymbol(c)
  }
  def Letter[_: P] = P( CharPred(c => isLetter(c) | isDigit(c) | c == '$' | c == '_' ) )
  def LetterDigitDollarUnderscore[_: P] =  P(
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
  def W[_: P](s: String) = P( s ~ !Basic.LetterDigitDollarUnderscore )(sourcecode.Name(s"`$s`"))
  // If the operator is followed by a comment, stop early so we can parse the comment
  def O[_: P](s: String) = P( s ~ (!Basic.OpChar | &(StringIn("/*", "//"))) )(sourcecode.Name(s"`$s`"))
}
