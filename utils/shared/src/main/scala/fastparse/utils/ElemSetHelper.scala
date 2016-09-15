package fastparse.utils

trait ElemSetHelper[Elem] {
  def toInt(a: Elem): Int
  val allValues: Seq[Elem]
}

object ElemSetHelper {
  implicit val CharBitSetHelper = new ElemSetHelper[Char] {
    override def toInt(a: Char): Int = a
    override val allValues = Char.MinValue to Char.MaxValue
  }


}
