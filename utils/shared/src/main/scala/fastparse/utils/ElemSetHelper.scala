package fastparse.utils
import acyclic.file

trait ElemSetHelper[Elem] {
  def toInt(a: Elem): Int
  def ordering: Ordering[Elem]
  def toLowerCase(in: Elem): Elem
  val allValues: Seq[Elem]

}
object ElemSetHelper {

  implicit object CharBitSetHelper extends ElemSetHelper[Char] {
    def toInt(a: Char): Int = a
    def ordering = implicitly[Ordering[Char]]
    def toLowerCase(in: Char) = in.toLower
    val allValues = Char.MinValue to Char.MaxValue
  }

}