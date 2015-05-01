package parsing

import parsing.Parser.Rule


/**
 * This provides an easy way to recurse over and transform a tree of [[Parser]]s
 * In particular, it provides an `apply` method that
 */
trait Walker{ w =>
  def recurseChildren[T](p1: Parser[T], stack: List[Parser[_]]) = {
    object ScopedWalker extends ScopedWalker{
      def apply[V](p2: Parser[V]) = w.recurse(p2, p1 :: stack)
    }
    p1.mapChildren(ScopedWalker)
  }
  def recurse[T](p: Parser[T], stack: List[Parser[_]]): Parser[T]
}

object RuleWalker extends parsing.Walker{
  def recurse[T](p: Parser[T], stack: List[Parser[_]]): Parser[T] = p match {
    case r: Rule[T] =>
      if (stack.contains(r)) r
      else recurse(r.pCached, r :: stack)
    case m => recurseChildren(m, stack)
  }
}
object EitherSequenceWalker extends parsing.Walker{
  def recurse[T](p: Parser[T], stack: List[Parser[_]]): Parser[T] = p match {
    case r: Parser.Either[T] => Parser.Either[T](Parser.Either.flatten(r.ps.toVector):_*)
    case m => recurseChildren(m, stack)
  }
}