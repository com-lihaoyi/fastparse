package fastparse

import fastparse.Utils.HexUtils

import scala.collection.mutable.ArrayBuffer

trait ElemTypeFormatter[ElemType] {
  val delimiter: ElemType
  val emptyElem: IndexedSeq[ElemType]
  def prettyPrint(input: IndexedSeq[ElemType]): String
  def literalize(input: IndexedSeq[ElemType]): String
}

trait ResultBuilder[ElemType, ResultType] {
  def convert(input: IndexedSeq[ElemType]): ResultType
}

object ElemTypeFormatter {

  implicit val CharFormatter = new ElemTypeFormatter[Char] {
    override val delimiter = '\n'
    override val emptyElem = IndexedSeq[Char]()
    override def prettyPrint(input: IndexedSeq[Char]): String = input.mkString
    override def literalize(input: IndexedSeq[Char]): String = Utils.literalize(input.mkString)
  }

  implicit val ByteFormatter = new ElemTypeFormatter[Byte] {
    override val delimiter = 0.toByte
    override val emptyElem = ArrayBuffer[Byte]()

    private def ByteToHex(b: Byte) = s"${HexUtils.hexChars((b & 0xf0) >> 4)}${HexUtils.hexChars(b & 15)}"

    override def prettyPrint(input: IndexedSeq[Byte]): String = input.map(ByteToHex).mkString(" ")
    override def literalize(input: IndexedSeq[Byte]): String = '"' + prettyPrint(input) + '"'
  }
}

object ResultBuilder {
  implicit val CharBuilder = new ResultBuilder[Char, String] {
    override def convert(input: IndexedSeq[Char]): String = input.mkString
  }

  implicit val ByteBuilder = new ResultBuilder[Byte, Array[Byte]] {
    override def convert(input: IndexedSeq[Byte]): Array[Byte] = input.toArray
  }
}