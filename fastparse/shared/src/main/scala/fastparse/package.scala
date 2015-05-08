import scala.collection.BitSet
import scala.language.experimental.macros
package object fastparse {
  implicit def enclosingFunctionName: FuncName = macro FuncName.impl

  val Pass = Parser.Pass
  val Fail = Parser.Fail
  val Start = Parser.Start
  val End = Parser.End
  val CharPred = Intrinsics.CharPred
  val CharIn = Intrinsics.CharIn
  val CharsWhile = Intrinsics.CharsWhile
  val StringIn = Intrinsics.StringIn
  val AnyChar = Parser.AnyChar

  object &{
    def apply(p: Parser[_]): Parser[Unit]  = Parser.Lookahead(p)
    def unapply(p: Parser[_]): Option[Parser[_]] = p match{
      case Parser.Lookahead(p) => Some(p)
      case _ => None
    }
  }

  implicit def wspStr(s: String) = if (s.length == 0) Parser.CharLiteral(s(0)) else Parser.Literal(s)

  def R[T](p: => Parser[T])(implicit name: FuncName): Parser[T] = Parser.Rule(name.name, () => p)
  type R0 = Parser[Unit]

  type R[+T] = Parser[T]
}
