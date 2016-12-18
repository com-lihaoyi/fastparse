package fastparse.sequence
import fastparse.utils._
import scala.reflect.ClassTag

case class VectorReprOps[T]()(implicit evidence: ClassTag[T]) extends ReprOps[T, Vector[T]] {
  type VecT = Vector[T]
  override def apply(input: VecT, i: Int) = input(i)
  override def slice(input: VecT, start: Int, end: Int) = input.slice(start, end)
  override def length(input: VecT) = input.length

  override def fromArray(input: Array[T]) = input.toVector
  override def fromSeq(input: Seq[T]) = input.toVector
  override def fromSingle(input: T) = Vector[T](input)
  override def toArray(input: VecT) = input.toArray
  override def flatten(input: Seq[VecT]) = input.flatten.toVector
  override def prettyPrint(input: VecT) = input.mkString("[", ", ", "]")
  override def literalize(input: VecT): String = prettyPrint(input)
  override def errorMessage(input: ParserInput[T, VecT], expected: String, idx: Int): String = {
    val locationCode = {
      val first = input.slice(idx - 5, idx)
      val last = input.slice(idx, idx + 5)
      val emptyString = ""
      prettyPrint(first) + prettyPrint(last) + "\n" + (" " * first.length) + "^"
    }
    val literal = literalize(input.slice(idx, idx + 20))
    s"found $literal, expected $expected at index $idx\n$locationCode"
  }
  override def prettyIndex(input: ParserInput[T, VecT], index: Int): String = {
    input match {
      case _ => String.valueOf(index)
    }
  }
}
