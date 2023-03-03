package scalaparse.syntax


import fastparse._
import fastparse._
import fastparse.NoWhitespace._
import CharPredicates._
import scalaparse.syntax.Identifiers.NamedFunction
object Basic {

  def UnicodeEscape[$: P] = P( "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits
  def Digit[$: P] = P( CharIn("0-9") )

  def HexDigit[$: P] = P( CharIn("0-9a-fA-F") )
  def HexNum[$: P] = P( "0x" ~ CharsWhileIn("0-9a-fA-F") )
  def DecNum[$: P] = P( CharsWhileIn("0-9") )
  def Exp[$: P] = P( CharIn("Ee") ~ CharIn("+\\-").? ~ DecNum )
  def FloatType[$: P] = P( CharIn("fFdD") )

  def WSChars[$: P] = P( NoTrace(CharsWhileIn("\u0020\u0009")) )
  def Newline[$: P] = P( NoTrace(StringIn("\r\n", "\n")) )
  def Semi[$: P] = P( ";" | Newline.rep(1) )
  def OpChar[$: P] = P ( CharPred(isOpChar) )

  val isOpChar = NamedFunction{
    case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' |
         ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' => true
    case c => isOtherSymbol(c) || isMathSymbol(c)
  }

  val LetterDigitDollarUnderscore = NamedFunction(
    c => isLetter(c) | isDigit(c) | c == '$' | c == '_'
  )
  val LowerChar = NamedFunction(
    c => isLower(c) || c == '$' | c == '_'
  )
  val UpperChar = NamedFunction(isUpper)

  def Lower[$: P] = P( CharPred(LowerChar) )
  def Upper[$: P] = P( CharPred(UpperChar) )
}
/**
 * Most keywords don't just require the correct characters to match,
 * they have to ensure that subsequent characters *don't* match in
 * order for it to be a keyword. This enforces that rule for key-words
 * (W) and key-operators (O) which have different non-match criteria.
 */
object Key {
  def W[$: P](s: String) = P( s ~ !CharPred(Basic.LetterDigitDollarUnderscore) )(s"`$s`", implicitly)
  // If the operator is followed by a comment, stop early so we can parse the comment
  def O[$: P](s: String) = P( s ~ (!Basic.OpChar | &(NoTrace(StringIn("/*", "//")))) )(s"`$s`", implicitly)
}
