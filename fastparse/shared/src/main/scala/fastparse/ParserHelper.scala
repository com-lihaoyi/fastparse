package fastparse

import scala.collection.mutable.ArrayBuffer

trait ParserHelper[ElemType] {
  val delimiter: ElemType
  val emptyElem: IndexedSeq[ElemType]
  def convertToString(input: IndexedSeq[ElemType]): String
  def literalize(input: IndexedSeq[ElemType]): String
}

trait ConversionHelper[ElemType, Repr] {
  def convert(input: IndexedSeq[ElemType]): Repr
}

object ParserHelper {

  implicit val StringParserHelper = new ParserHelper[Char] {
    override val delimiter = '\n'
    override val emptyElem = IndexedSeq[Char]()
    override def convertToString(input: IndexedSeq[Char]): String = input.mkString
    override def literalize(input: IndexedSeq[Char]): String = Utils.literalize(input.mkString)
  }

  implicit val ByteParserHelper = new ParserHelper[Byte] {
    override val delimiter = 0.toByte
    override val emptyElem = ArrayBuffer[Byte]()
    override def convertToString(input: IndexedSeq[Byte]): String = input.mkString
    override def literalize(input: IndexedSeq[Byte]): String = convertToString(input)
  }
}

object ConversionHelper {
  implicit val StringConversionHelper = new ConversionHelper[Char, String] {
    override def convert(input: IndexedSeq[Char]): String = input.mkString
  }

  implicit val ByteConversionHelper = new ConversionHelper[Byte, Array[Byte]] {
    override def convert(input: IndexedSeq[Byte]): Array[Byte] = input.toArray
  }
}