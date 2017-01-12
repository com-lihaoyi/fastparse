package pythonparse
import acyclic.file
object  WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.wscomment)
/**
 * Python's lexical grammar; how basic tokens get parsed. This stuff is
 * sensitive to whitespace, which can only appear where it's explicitly
 * stated to be part of the grammar.
 *
 * Manually transcribed from https://docs.python.org/2/reference/lexical_analysis.html
 */
object Lexical {
  import fastparse.all._
  def kw(s: String) = s ~ !(letter | digit | "_")
  val comment = P( "#" ~ CharsWhile(_ != '\n', min = 0) )
  val wscomment = P( (CharsWhile(" \n".toSet, min = 1) | Lexical.comment | "\\\n").rep )
  val nonewlinewscomment = P( (CharsWhile(" ".toSet, min = 1) | Lexical.comment | "\\\n").rep )

  val identifier: P[Ast.identifier] =
    P( (letter|"_") ~ (letter | digit | "_").rep ).!.filter(!keywordList.contains(_)).map(Ast.identifier)
  val letter     = P( lowercase | uppercase )
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val digit      = P( CharIn('0' to '9') )

  val keywordList = Set(
    "and",       "del",       "from",      "not",       "while",
    "as",        "elif",      "global",    "or",        "with",
    "assert",    "else",      "if",        "pass",      "yield",
    "break",     "except",    "import",    "print",
    "class",     "exec",      "in",        "raise",
    "continue",  "finally",   "is",        "return",
    "def",       "for",       "lambda",    "try"
  )

  val stringliteral: P[String] = P( stringprefix.? ~ (longstring | shortstring) )
  val stringprefix: P0 = P(
    "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR"
  )
  val shortstring: P[String] = P( shortstring0("'") | shortstring0("\"") )
  def shortstring0(delimiter: String) = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P( shortstringchar(quote) | escapeseq )
  def shortstringchar(quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

  val longstring: P[String] = P( longstring0("'''") | longstring0("\"\"\"") )
  def longstring0(delimiter: String) = P( delimiter ~ longstringitem(delimiter).rep.! ~ delimiter)
  def longstringitem(quote: String): P0 = P( longstringchar(quote) | escapeseq | !quote ~ quote.take(1)  )
  def longstringchar(quote: String): P0 = P( CharsWhile(!s"\\${quote(0)}".contains(_)) )

  val escapeseq: P0 = P( "\\" ~ AnyChar )

  def negatable[T](p: P[T])(implicit ev: Numeric[T]) = (("+" | "-").?.! ~ p).map {
    case ("-", i) => ev.negate(i)
    case (_, i) => i
  }

  val longinteger: P[BigInt] = P( integer ~ ("l" | "L") )
  val integer: P[BigInt] = negatable[BigInt](P( octinteger | hexinteger | bininteger | decimalinteger))
  val decimalinteger: P[BigInt] = P( nonzerodigit ~ digit.rep | "0" ).!.map(scala.BigInt(_))
  val octinteger: P[BigInt] = P( "0" ~ ("o" | "O") ~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).! ).map(scala.BigInt(_, 8))
  val hexinteger: P[BigInt] = P( "0" ~ ("x" | "X") ~ hexdigit.rep(1).! ).map(scala.BigInt(_, 16))
  val bininteger: P[BigInt] = P( "0" ~ ("b" | "B") ~ bindigit.rep(1).! ).map(scala.BigInt(_, 2))
  val nonzerodigit: P0 = P( CharIn('1' to '9') )
  val octdigit: P0 = P( CharIn('0' to '7') )
  val bindigit: P0 = P( "0" | "1" )
  val hexdigit: P0 = P( digit | CharIn('a' to 'f', 'A' to 'F') )


  val floatnumber: P[BigDecimal] = negatable[BigDecimal](P( pointfloat | exponentfloat ))
  val pointfloat: P[BigDecimal] = P( intpart.? ~ fraction | intpart ~ "." ).!.map(BigDecimal(_))
  val exponentfloat: P[BigDecimal] = P( (intpart | pointfloat) ~ exponent ).!.map(BigDecimal(_))
  val intpart: P[BigDecimal] = P( digit.rep(1) ).!.map(BigDecimal(_))
  val fraction: P0 = P( "." ~ digit.rep(1) )
  val exponent: P0 = P( ("e" | "E") ~ ("+" | "-").? ~ digit.rep(1) )


  val imagnumber = P( (floatnumber | intpart) ~ ("j" | "J") )
}
