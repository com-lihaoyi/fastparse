package fastparse
import language.experimental.macros
import fastparse.parsers.{Intrinsics, Terminals}
import acyclic.file

/**
 * This is basically a trait which contains
 * the "public" API to fastparse packages
 */
trait Api{

  val Parsed = core.Parsed
  type Parsed[+T] = core.Parsed[T, Char]
  type Failure = Parsed.Failure[Char]
  type Success[T] = Parsed.Success[T, Char]
  
  val Pass = parsers.Terminals.Pass[Char, String]()
  val Fail = parsers.Terminals.Fail[Char, String]()
  val Start = parsers.Terminals.Start[Char, String]()
  val End = parsers.Terminals.End[Char, String]()
  val Index = parsers.Terminals.Index[Char, String]()
  val AnyChar = parsers.Terminals.AnyChar[Char, String]()
  val IgnoreCase = parsers.Terminals.IgnoreCase[String] _

  val CharPred = Intrinsics.CharPred[String] _
  def CharIn(strings: Seq[Char]*) = Intrinsics.CharIn[String](strings: _*)
  def CharsWhile(pred: Char => Boolean, min: Int = 1) = Intrinsics.CharsWhile[String](pred, min)
  val CharPredicates = fastparse.CharPredicates
  def StringIn(strings: String*) = Intrinsics.StringIn[String](strings: _*)

  val NoTrace = parsers.Combinators.NoTrace
  val NoCut = parsers.Combinators.NoCut

  val & = parsers.Combinators.Lookahead

  implicit def wspStr(s: String): P0 =
    if (s.length == 1) parsers.Terminals.CharLiteral(s(0))
    else parsers.Terminals.Literal(s)

  def P[T](p: => Parser[T])(implicit name: sourcecode.Name): Parser[T] =
    parsers.Combinators.Rule(name.value, () => p)

  type P0 = Parser[Unit]
  type Parser[+T] = core.Parser[T, Char, String]
  type P[+T] = Parser[T]
  val ParseError = core.ParseError[Char] _
  type ParseError = core.ParseError[Char]
}
object all extends Api{
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Char, String]): ParserApi[V, Char, String] =
    new ParserApiImpl[V, Char, String](p)
}
object noApi extends Api