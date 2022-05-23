package pythonparse
import fastparse.NoWhitespace._
/**
 * Python's lexical grammar; how basic tokens get parsed. This stuff is
 * sensitive to whitespace, which can only appear where it's explicitly
 * stated to be part of the grammar.
 *
 * Manually transcribed from https://docs.python.org/2/reference/lexical_analysis.html
 */
object Lexical {
  import fastparse._

  def kw[_p: P](s: String) = s ~ !(letter | digit | "_")
  def comment[_p: P] = P( "#" ~ CharsWhile(_ != '\n', 0) )
  def wscomment[_p: P] = P( (CharsWhileIn(" \n") | Lexical.comment | "\\\n").rep )
  def nonewlinewscomment[_p: P] = P( (CharsWhileIn(" ") | Lexical.comment | "\\\n").rep )

  def identifier[_p: P]: P[Ast.identifier] =
    P( (letter|"_") ~ (letter | digit | "_").rep ).!.filter(!keywordList.contains(_)).map(Ast.identifier.apply)
  def letter[_p: P]     = P( lowercase | uppercase )
  def lowercase[_p: P]  = P( CharIn("a-z") )
  def uppercase[_p: P]  = P( CharIn("A-Z") )
  def digit[_p: P]      = P( CharIn("0-9") )

  val keywordList = Set(
    "and",       "del",       "from",      "not",       "while",
    "as",        "elif",      "global",    "or",        "with",
    "assert",    "else",      "if",        "pass",      "yield",
    "break",     "except",    "import",    "print",
    "class",     "exec",      "in",        "raise",
    "continue",  "finally",   "is",        "return",
    "def",       "for",       "lambda",    "try"
  )

  def stringliteral[_p: P]: P[String] = P( stringprefix.? ~ (longstring | shortstring) )
  def stringprefix[_p: P]: P[Unit] = P(
    "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR"
  )
  def shortstring[_p: P]: P[String] = P( shortstring0("'") | shortstring0("\"") )
  def shortstring0[_p: P](delimiter: String) = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem[_p: P](quote: String): P[Unit] = P( shortstringchar(quote) | escapeseq )
  def shortstringchar[_p: P](quote: String): P[Unit] = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

  def longstring[_p: P]: P[String] = P( longstring0("'''") | longstring0("\"\"\"") )
  def longstring0[_p: P](delimiter: String) = P( delimiter ~ longstringitem(delimiter).rep.! ~ delimiter)
  def longstringitem[_p: P](quote: String): P[Unit] = P( longstringchar(quote) | escapeseq | !quote ~ quote.take(1)  )
  def longstringchar[_p: P](quote: String): P[Unit] = P( CharsWhile(!s"\\${quote(0)}".contains(_)) )

  def escapeseq[_p: P]: P[Unit] = P( "\\" ~ AnyChar )

  def negatable[T, _p: P](p: => P[T])(implicit ev: Numeric[T]) = (("+" | "-").?.! ~ p).map {
    case ("-", i) => ev.negate(i)
    case (_, i) => i
  }

  def longinteger[_p: P]: P[BigInt] = P( integer ~ ("l" | "L") )
  def integer[_p: P]: P[BigInt] = negatable[BigInt, Any](P( octinteger | hexinteger | bininteger | decimalinteger))
  def decimalinteger[_p: P]: P[BigInt] = P( nonzerodigit ~ digit.rep | "0" ).!.map(scala.BigInt(_))
  def octinteger[_p: P]: P[BigInt] = P( "0" ~ ("o" | "O") ~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).! ).map(scala.BigInt(_, 8))
  def hexinteger[_p: P]: P[BigInt] = P( "0" ~ ("x" | "X") ~ hexdigit.rep(1).! ).map(scala.BigInt(_, 16))
  def bininteger[_p: P]: P[BigInt] = P( "0" ~ ("b" | "B") ~ bindigit.rep(1).! ).map(scala.BigInt(_, 2))
  def nonzerodigit[_p: P]: P[Unit] = P( CharIn("1-9") )
  def octdigit[_p: P]: P[Unit] = P( CharIn("0-7") )
  def bindigit[_p: P]: P[Unit] = P( "0" | "1" )
  def hexdigit[_p: P]: P[Unit] = P( digit | CharIn("a-f", "A-F") )


  def floatnumber[_p: P]: P[BigDecimal] = negatable[BigDecimal, Any](P( pointfloat | exponentfloat ))
  def pointfloat[_p: P]: P[BigDecimal] = P( intpart.? ~ fraction | intpart ~ "." ).!.map(BigDecimal(_))
  def exponentfloat[_p: P]: P[BigDecimal] = P( (intpart | pointfloat) ~ exponent ).!.map(BigDecimal(_))
  def intpart[_p: P]: P[BigDecimal] = P( digit.rep(1) ).!.map(BigDecimal(_))
  def fraction[_p: P]: P[Unit] = P( "." ~ digit.rep(1) )
  def exponent[_p: P]: P[Unit] = P( ("e" | "E") ~ ("+" | "-").? ~ digit.rep(1) )


  def imagnumber[_p: P] = P( (floatnumber | intpart) ~ ("j" | "J") )
}
