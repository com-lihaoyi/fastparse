import scala.collection.BitSet
import scala.language.experimental.macros
package object parsing {
  implicit def enclosingFunctionName: FuncName = macro FuncName.impl

  val Pass = Parser.Pass
  val Fail = Parser.Fail
  val CharPred = Parser.CharPred
  val Dispatcher = Parser.CharTrie
  def CharSets(sets: Seq[Char]*) = {
    val uberSet = BitSet(sets.flatten.map(_.toInt):_*)
    CharPred((c: Char) => uberSet(c.toInt))
  }
  def &(p: Parser[_]): Parser[Unit] = Parser.Lookahead(p)

  implicit def wspStr(s: String) = if (s.length == 0) Parser.CharLiteral(s(0)) else Parser.Literal(s)

  def rule[T](p: => Parser[T])(implicit name: FuncName): Parser[T] = Parser.Named(name.name, () => p)

  type Rule0 = Parser[_]
}
