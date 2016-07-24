package fastparse

import scala.collection.mutable.ArrayBuffer

abstract class ParserInputLogger {
  def logDropBuffer(index: Int)
  def logApply(index: Int)
}

abstract class ParserInput[ElemType] {
  def apply(index: Int): ElemType
  def dropBuffer(index: Int): Unit
  def slice(from: Int, until: Int): IndexedSeq[ElemType]
  def length: Int
  def innerLength: Int
  def isReachable(index: Int): Boolean
}

abstract class LoggedParsedInput[ElemType](parserInput: ParserInput[ElemType]) extends ParserInput[ElemType] {

  def logApply(index: Int): Unit = ()
  def logDropBuffer(index: Int): Unit = ()
  def logIsReachable(index: Int): Unit = ()
  def logSlice(from: Int, until: Int): Unit = ()

  def apply(index: Int) = {
    logApply(index)
    parserInput.apply(index)
  }
  def dropBuffer(index: Int) = {
    logDropBuffer(index)
    parserInput.dropBuffer(index)
  }
  def slice(from: Int, until: Int) = {
    logSlice(from, until)
    parserInput.slice(from, until)
  }
  def length = parserInput.length
  def innerLength = parserInput.innerLength
  def isReachable(index: Int) = {
    logIsReachable(index)
    parserInput.isReachable(index)
  }
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
  private var buffer: Vector[ElemType] = Vector()
  private var firstIdx: Int = 0 // index in the data corresponding to the 0th element in the buffer

  private def requestUntil(until: Int): Boolean = {
    while (this.length <= until && data.hasNext) {
      val chunk = data.next()
      buffer = buffer ++ chunk
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
