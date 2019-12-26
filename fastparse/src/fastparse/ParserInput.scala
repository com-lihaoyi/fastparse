package fastparse

import java.io.InputStreamReader

import fastparse.internal.{UberBuffer, Util}


/**
  * Trait that represents classes with isReachable method
  *
  * Currently the only use of it is to avoid the cyclic dependencies between Utils and ParserInput
  */

trait IsReachable {
  def apply(index: Int): Char
  def isReachable(index: Int): Boolean
}

trait ParserInputSource{
  def parseThrough[T](f: ParserInput => T): T
}
object ParserInputSource extends ParserInputSourceLowPri {

  implicit class fromParserInput[T](t: T)(implicit conv: T => ParserInput) extends ParserInputSource {
    def parseThrough[T](f: ParserInput => T) = f(conv(t))
  }

}
trait ParserInputSourceLowPri{

  implicit def fromReadable[T](s: T)(implicit f: T => geny.Readable) = FromReadable(
    f(s),
    // Default bufferSize of 4096. Somewhat arbitrary, but doesn't seem to matter 
    // much in benchmarks, e.g. on parsing `GenJSCode.scala`:
    //
    // Input \ Iterations        1   2   3   4   5
    // Indexed String            360 455 458 460 461
    // Streaming String 2048     208 291 294 293 291
    // Streaming String 4096     206 282 287 289 279
    // Streaming String 8192     234 303 302 305 298
    // Streaming String 16384    232 301 307 306 308
    // Streaming File 2048       222 293 295 294 293
    // Streaming File 4096       223 296 302 303 303
    // Streaming File 8192       219 290 298 298 297
    // Streaming File 16384      225 301 303 304 304
    bufferSize = 4096
  )
  case class FromReadable(s: geny.Readable, bufferSize: Int) extends ParserInputSource {
    def parseThrough[T](f: ParserInput => T): T = {
      s.readBytesThrough(is => f(ReaderParserInput(new InputStreamReader(is), bufferSize)))
    }
  }
}

object ParserInput{
  implicit def fromString(s: String) = IndexedParserInput(s)
  implicit def FromIterator(s: Iterator[String]) = IteratorParserInput(s)
}
/**
  * ParserInput class represents data that is needed to parse.
  *
  * It can be regular `IndexedSeq` that behaves as simple array or
  * `Iterator` of `IndexedSeq` batches which is optimized by `dropBuffer` method.
  */
abstract class ParserInput extends IsReachable {
  def apply(index: Int): Char

  /**
    * Special method for `Iterator` mode. It drops the prefix of the internal buffer
    * so that all the data '''strictly before''' becomes unavailable and `index` is the first valid element to access.
    */
  def dropBuffer(index: Int): Unit

  /**
    * @return Slice of internal data.
    *         For `IndexedSeq` mode it works as regular slice, if `until` overshoots the end of input,
    *         it just ignores it and behaves like `until` equals to the length of input.
    *         Same for `Iterator` mode, but it requests batches while the index of last retrieved element is less than `until`
    *         and if `until` is farther away than any element, it ignores this too.
    */
  def slice(from: Int, until: Int): String

  def length: Int
  def innerLength: Int

  /**
    * Shows if we can access to the element at given `index`.
    */
  def isReachable(index: Int): Boolean

  def checkTraceable(): Unit

  def prettyIndex(index: Int): String
}

case class IndexedParserInput(data: String) extends ParserInput {
  override def apply(index: Int) = data.charAt(index)

  /**
    * As for `IndexedSeq` mode `dropBuffer` does nothing.
    */
  override def dropBuffer(index: Int) = {}

  override def slice(from: Int, until: Int) = data.slice(from, until)

  /**
    * @return Length of internal immutable data. It works equally as `innerLength`.
    */
  override def length: Int = data.length
  /**
    * @return Length of internal immutable data. It works equally as `length`.
    */
  override def innerLength: Int = length

  /**
    * Simple condition of `index` < `length`
    */
  override def isReachable(index: Int): Boolean = index < length

  def checkTraceable() = ()

  private[this] lazy val lineNumberLookup = Util.lineNumberLookup(data)
  def prettyIndex(index: Int): String = {
    val line = lineNumberLookup.indexWhere(_ > index) match{
      case -1 => lineNumberLookup.length - 1
      case n => math.max(0, n - 1)
    }
    val col = index - lineNumberLookup(line)
    s"${line+1}:${col+1}"
  }
}

trait BufferedParserInput extends ParserInput{
  protected val buffer: UberBuffer = new UberBuffer(16)
  protected var firstIdx: Int = 0 // index in the data corresponding to the 0th element in the buffer

  protected def requestUntil(until: Int): Boolean
  /**
    * Requests batches until given `index` and in case of success returns needed element.
    */
  override def apply(index: Int): Char = {
    requestUntil(index)
    buffer(index - firstIdx)
  }

  /**
    * Drops all elements before index not inclusive.
    */
  override def dropBuffer(index: Int): Unit = {
    if (index > firstIdx) {
      buffer.drop(index - firstIdx)
      firstIdx = index
    }
  }

  /**
    * Also requests batches to the `until`.
    * If the current buffer is too small to provide some part of data after `from` bound,
    * lower bound of slice is cut to the minimum of `from` and first index of accessible element in the buffer.
    */
  override def slice(from: Int, until: Int): String = {
    requestUntil(until - 1)
    val lo = math.max(from, firstIdx)
    new String(buffer.slice(lo - firstIdx, until - firstIdx))
  }

  /**
    * @return Index of the last element which buffer contains.
    */
  override def length: Int = firstIdx + buffer.length

  /**
    * @return Length of the internal buffer.
    */
  override def innerLength: Int = buffer.length

  /**
    * Requests batches until index of last retrieved element is less than `index` and,
    * when it's possible, returns true, otherwise false.
    */
  override def isReachable(index: Int): Boolean = {
    index < length || requestUntil(index)
  }

  def checkTraceable() = throw new RuntimeException(
    s"Cannot perform `.traced` on an `${getClass.getName}`, as it needs to parse " +
      "the input a second time to collect traces, which is impossible after an " +
      "`IteratorParserInput` is used once and the underlying Iterator exhausted."
  )

  def prettyIndex(index: Int): String = {
    String.valueOf(index)
  }

}
/**
  * Contains buffer - queue of elements that extends from given iterator when particular elements are requested;
  * and shrinks by calling `dropBuffer` method.
  *
  * Generally, at any specific time this buffer contains "suffix" of given iterator,
  * e.g. some piece of data from past calls of `next`, which extends by requesting new batches from iterator.
  * Therefore we can denote the same notation of indices as for regular `Array` or more abstract `IndexedSeq`.
  * The only difference is when index doesn't fit into the bounds of current buffer
  * either the new batches are requested to extend the buffer, either it's inaccessible at all,
  * so calling of `dropBuffer` should guarantee that there won't be any attempts to access to the elements in dropped part of input.
  */
case class IteratorParserInput(data: Iterator[String]) extends BufferedParserInput{
  protected def requestUntil(until: Int): Boolean = {
    while (this.length <= until && data.hasNext) {
      val chunk = data.next()
      val chars = chunk.toCharArray
      buffer.write(chars, chars.length)
    }
    this.length > until
  }
}

/**
  * A [[ParserInput]] that pulls data from a given `java.io.Reader`. Typically
  * not used alone, and instead is used as part of a [[ParserInputSource.FromReadable]]
  */
case class ReaderParserInput(data: java.io.Reader, bufferSize: Int) extends BufferedParserInput{

  private val streamBuffer = new Array[Char](bufferSize)
  protected def requestUntil(until: Int): Boolean = {
    var empty = false
    while (this.length <= until && !empty) {
      data.read(streamBuffer) match{
        case -1 => empty = true
        case n => buffer.write(streamBuffer, n)
      }
    }
    this.length > until
  }
}