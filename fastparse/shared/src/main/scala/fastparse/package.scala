import fastparse.parsers.Intrinsics


import scala.language.experimental.macros
package object fastparse {
  implicit def enclosingFunctionName: Utils.FuncName = macro Utils.FuncName.impl

  val Result = core.Result

  val Pass = parsers.Terminals.Pass
  val Fail = parsers.Terminals.Fail
  val Start = parsers.Terminals.Start
  val End = parsers.Terminals.End
  val Index = parsers.Terminals.Index
  val AnyChar = parsers.Terminals.AnyChar
  val IgnoreCase = parsers.Terminals.LiteralIgnoreCase

  val CharPred = Intrinsics.CharPred
  val CharIn = Intrinsics.CharIn
  val CharsWhile = Intrinsics.CharsWhile
  val StringIn = Intrinsics.StringIn

  val & = parsers.Combinators.Lookahead

  implicit def wspStr(s: String) =
    if (s.length == 0) parsers.Terminals.CharLiteral(s(0))
    else parsers.Terminals.Literal(s)

  def P[T](p: => Parser[T])(implicit name: Utils.FuncName): Parser[T] =
    parsers.Combinators.Rule(name.name, () => p)

  type P0 = Parser[Unit]

  type P[+T] = Parser[T]
}
