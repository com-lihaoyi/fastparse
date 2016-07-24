package fastparse

import scala.collection.mutable.ArrayBuffer

abstract class ParserInput[ElemType] {
  def apply(index: Int): ElemType
  def dropBuffer(index: Int): Unit
  def slice(from: Int, until: Int): IndexedSeq[ElemType]
  def length: Int
  def innerLength: Int
  def isReachable(index: Int): Boolean
}

case class IndexedParserInput[ElemType](data: IndexedSeq[ElemType]) extends ParserInput[ElemType] {
  override def apply(index: Int) = data(index)
  override def dropBuffer(index: Int) = {}
  override def slice(from: Int, until: Int): IndexedSeq[ElemType] = data.slice(from, until)
  override def length: Int = data.length
  override def innerLength: Int = length
  override def isReachable(index: Int): Boolean = index < length
}

final case class IteratorParserInput[ElemType](data: Iterator[IndexedSeq[ElemType]]) extends ParserInput[ElemType] {
  private var buffer: ArrayBuffer[ElemType] = ArrayBuffer()
  private var firstIdx: Int = 0 // index in the data corresponding to the 0th element in the buffer

  private def requestUntil(until: Int): Boolean = {
    while (this.length <= until && data.hasNext) {
      val chunk = data.next()
      buffer ++= chunk
    }
    this.length > until
  }

  override def apply(index: Int): ElemType = {
    requestUntil(index)
    buffer(index - firstIdx)
  }

  override def dropBuffer(index: Int): Unit = { // drops all elements before index not inclusive
    if (index > firstIdx) {
      buffer = buffer.drop(index - firstIdx)
      firstIdx = index
    }
  }

  override def slice(from: Int, until: Int): IndexedSeq[ElemType] = {
    requestUntil(until - 1)
    val lo = math.max(from, firstIdx)
    buffer.slice(lo - firstIdx, until - firstIdx)
  }

  override def length: Int = firstIdx + buffer.length

  override def innerLength: Int = buffer.length

  override def isReachable(index: Int): Boolean = {
    index < length || requestUntil(index)
  }
}
