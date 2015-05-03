import scala.collection.BitSet
import scala.language.experimental.macros
package object parsing {
  implicit def enclosingFunctionName: FuncName = macro FuncName.impl

  val Pass = Parser.Pass
  val Fail = Parser.Fail
  val Start = Parser.Start
  val End = Parser.End
  val CharPred = Parser.CharPred
  val CharIn = Parser.CharIn
  val StringIn = Parser.StringIn
  val AnyChar = Parser.AnyChar

  def &(p: Parser[_]): Parser[Unit] = Parser.Lookahead(p)

  implicit def wspStr(s: String) = if (s.length == 0) Parser.CharLiteral(s(0)) else Parser.Literal(s)

  def R[T](p: => Parser[T])(implicit name: FuncName): Parser[T] = Parser.Rule(name.name, () => p)
  type R0 = Parser[Unit]

  type R[+T] = Parser[T]
}
