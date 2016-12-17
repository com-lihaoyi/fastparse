package fastparse.sequence
import fastparse.utils._
import scala.reflect.ClassTag

case class VectorReprOps[T]()(implicit evidence: ClassTag[T]) extends fastparse.utils.ReprOps[T, Vector[T]] {
  type U = Vector[T]
  val U = Vector
  override def apply(input: U, i: Int) = input(i)
  override def slice(input: U, start: Int, end: Int) = input.slice(start, end)
  override def length(input: U) = input.length

  override def fromArray(input: Array[T]) = input.toVector
  override def fromSeq(input: Seq[T]) = input.toVector
  override def fromSingle(input: T) = U(input)
  override def toArray(input: U) = input.toArray
  override def flatten(input: Seq[U]) = input.flatten.toVector
  override def prettyPrint(input: U) = input.mkString("[", ", ", "]")
  override def literalize(input: U): String = prettyPrint(input)
  override def errorMessage(input: fastparse.utils.ParserInput[T, U], expected: String, idx: Int): String = {
    val locationCode = {
      val first = input.slice(idx - 5, idx)
      val last = input.slice(idx, idx + 5)
      val emptyString = ""
      prettyPrint(first) + prettyPrint(last) + "\n" + (" " * first.length) + "^"
    }
    val literal = literalize(input.slice(idx, idx + 20))
    s"found $literal, expected $expected at index $idx\n$locationCode"
  }
  override def prettyIndex(input: fastparse.utils.ParserInput[T, U], index: Int): String = {
    input match {
      case _ => String.valueOf(index)
    }
  }
}
