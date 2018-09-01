package fastparse.sequence
import fastparse.utils._
import scala.reflect.ClassTag

case class SeqReprOps[T]()(implicit evidence: ClassTag[T]) extends ReprOps[T, Seq[T]] {
  type Lst = Seq[T]
  override def apply(input: Lst, i: Int) = input(i)
  override def slice(input: Lst, start: Int, end: Int) = input.slice(start, end)
  override def length(input: Lst) = input.length

  override def fromArray(input: Array[T]) = input.toSeq
  override def fromSeq(input: Seq[T]) = input.toSeq
  override def fromSingle(input: T) = Seq(input)
  override def toArray(input: Lst) = input.toArray
  override def flatten(input: Seq[Lst]) = input.flatten
  override def prettyPrint(input: Lst) = input.mkString("[", ", ", "]")
  override def literalize(input: Lst): String = prettyPrint(input)
  override def errorMessage(input: ParserInput[T, Lst], expected: String, idx: Int): String = {
    val locationCode = {
      val first = input.slice(idx - 5, idx)
      val last = input.slice(idx, idx + 5)
      val emptyString = ""
      prettyPrint(first) + prettyPrint(last) + "\n" + (" " * first.length) + "^"
    }
    val literal = literalize(input.slice(idx, idx + 20))
    s"found $literal, expected $expected at index $idx\n$locationCode"
  }
  override def prettyIndex(input: ParserInput[T, Lst], index: Int): String = {
    input match {
      case _ => String.valueOf(index)
    }
  }
}
