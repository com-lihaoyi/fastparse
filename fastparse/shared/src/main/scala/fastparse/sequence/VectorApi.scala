package fastparse.sequence

import fastparse.core.{ ParserApi, ParserApiImpl }
import fastparse.parsers.Terminals.AnyElems
import scala.reflect.ClassTag
import fastparse.{ core, parsers }

class VectorApi[A]()(implicit evidence: ClassTag[A]) extends SeqApi[A, Vector[A]](implicitly, VectorReprOps[A]) {
  val AnyElem = parsers.Terminals.AnyElem[A, Vector[A]]("AnyElem")
  def AnyElem(count: Int) = AnyElems[A, Vector[A]]("AnyElems", count)
  def ElemPred(pred: A => Boolean): P0 = SeqItemParsers.Intrinsics.ElemPred[A, Vector[A]]("ElemPred", pred)
  def ElemIn(items: Seq[A]*): P0 = SeqItemParsers.Intrinsics.ElemIn[A, Vector[A]]("ElemIn", items: _*)
  def ElemsWhile(pred: A => Boolean, min: Int = 1) = SeqItemParsers.Intrinsics.ElemsWhile[A, Vector[A]]("ElemsWhile", pred, min)
}

/**
 * This class is used to parse a Vector of items
 *
 * People can use it if they needs to Parse a Vector resulting of a preceding
 * text parsed for example or to parse any Vector of data.
 *
 * For example if we need to parse an address,
 * we can have a string parser that produces this sequence :
 */
class allVector[A]()(implicit evidence: ClassTag[A]) extends VectorApi[A]() {
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, A, Vector[A]]): ParserApi[V, A, Vector[A]] =
    new ParserApiImpl[V, A, Vector[A]](p)
}
