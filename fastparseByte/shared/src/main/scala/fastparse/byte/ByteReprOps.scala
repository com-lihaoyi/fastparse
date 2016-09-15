package fastparse.byte
import acyclic.file
import fastparse.utils.{ElemSetHelper, ParserInput, ReprOps}
import fastparse.utils.Utils.HexUtils
import scodec.bits.ByteVector

object ByteBitSetHelper extends ElemSetHelper[Byte] {
  def toInt(a: Byte): Int = a
  def ordering = implicitly[Ordering[Byte]]
  def toLowerCase(in: Byte) = in
  val allValues = (Byte.MinValue.toInt to Byte.MaxValue.toInt).map(_.toByte)
}

object ByteReprOps extends ReprOps[Byte, ByteVector] {
  private[this] type Bytes = ByteVector
  def fromArray(input: Array[Byte]) = ByteVector(input:_*)
  def toArray(input: ByteVector) = input.toArray
  def fromSeq(input: Seq[Byte]) = ByteVector(input:_*)

  private def ByteToHex(b: Byte) = s"${HexUtils.hexChars((b & 0xf0) >> 4)}${HexUtils.hexChars(b & 15)}"

  def prettyPrint(input: Bytes): String = input.toArray.map(ByteToHex).mkString(" ")
  def literalize(input: Bytes): String = '"' + prettyPrint(input) + '"'

  def errorMessage(input: ParserInput[Byte, Bytes], expected: String, idx: Int): String = {
    val locationCode = {
      val first = input.slice(idx - 20, idx)
      val last = input.slice(idx, idx + 20)

      prettyPrint(first) + prettyPrint(last) + "\n" + (" " * length(first)) + "^"
    }
    val literal = literalize(input.slice(idx, idx + 20))
    s"found $literal, expected $expected at index $idx\n$locationCode"
  }

  def prettyIndex(input: ParserInput[Byte, Bytes], index: Int): String = String.valueOf(index)

  def slice(value: Bytes, start: Int, end: Int) = {
    value.slice(math.max(0, start), math.min(end, value.length))
  }

  def apply(value: Bytes, i: Int) = value(i)

  def length(value: Bytes) = value.length.toInt

  def fromSingle(input: Byte) = ByteVector(input)

  def flatten(input: Seq[Bytes]): Bytes = {
    var current = ByteVector.empty
    for(i <- input) current = current ++ i
    current
  }
}