import scala.collection.BitSet
import scala.language.experimental.macros
package object parsing {
  implicit def enclosingFunctionName: FuncName = macro FuncName.impl

  val Pass = Parser.Pass
  val Fail = Parser.Fail
  val CharPred = Parser.CharPred
  val CharSets = Parser.CharSets
  val Dispatcher = Parser.CharTrie
  val AnyChar = Parser.AnyChar

  def &(p: Parser[_]): Parser[Unit] = Parser.Lookahead(p)

  implicit def wspStr(s: String) = if (s.length == 0) Parser.CharLiteral(s(0)) else Parser.Literal(s)

  def R[T](p: => Parser[T])(implicit name: FuncName): Parser[T] = Parser.Named(name.name, () => p)

  type Parser0 = Parser[_]
}
