package fastparse.core
import acyclic.file
/**
 * Something which contains an operator precedence, which can be used
 * to correctly wrap other things which contain lower precedences in 
 * parentheses when stringifying.
 */
trait Precedence{
  protected def opPred: Int
  protected def opWrap(s: Precedence) = Precedence.opWrap(s, opPred)
}
/**
 * All the level of operator precedence in Scala
 */
object Precedence {
  def opWrap(s: Precedence, selfOpPred: Int) = {
    if (s.opPred >= selfOpPred) s.toString
    else "(" + s + ")"
  }
  val Letters = 0
  val | = 1
  val ^ = 2
  val & = 3
  val <> = 4
  val =! = 5
  val `:` = 6
  val +- = 7
  val */% = 8
  val OtherOp = 9
  val PrefixOp = 10
  val Max = 11

}
