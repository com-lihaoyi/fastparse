package fastparse
import language.experimental.macros
import fastparse.parsers.Intrinsics
import acyclic.file

/**
 * This is basically a trait which contains
 * the "public" API to fastparse packages
 */
trait Api{
  implicit def enclosingFunctionName: Utils.FuncName = macro MacroUtils.impl

  val Parsed = core.Parsed
  type Parsed[+T] = core.Parsed[T]
  
  val Pass = parsers.Terminals.Pass
  val Fail = parsers.Terminals.Fail
  val Start = parsers.Terminals.Start
  val End = parsers.Terminals.End
  val Index = parsers.Terminals.Index
  val AnyChar = parsers.Terminals.AnyChar
  val IgnoreCase = parsers.Terminals.IgnoreCase

  val CharPred = Intrinsics.CharPred
  val CharIn = Intrinsics.CharIn
  val CharsWhile = Intrinsics.CharsWhile
  val CharPredicates = fastparse.CharPredicates
  val StringIn = Intrinsics.StringIn

  val NoTrace = parsers.Combinators.NoTrace
  val NoCut = parsers.Combinators.NoCut

  val & = parsers.Combinators.Lookahead

  implicit def wspStr(s: String): P0 =
    if (s.length == 1) parsers.Terminals.CharLiteral(s(0))
    else parsers.Terminals.Literal(s)

  def P[T](p: => Parser[T])(implicit name: Utils.FuncName): Parser[T] =
    parsers.Combinators.Rule(name.name, () => p)

  type P0 = Parser[Unit]
  type Parser[+T] = core.Parser[T]
  type P[+T] = Parser[T]
  val ParseError = core.ParseError
  type ParseError = core.ParseError
}
object all extends Api{
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V]): ParserApi[V] =
    new ParserApiImpl[V](p)
}
object noApi extends Api