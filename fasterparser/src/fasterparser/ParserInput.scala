package fasterparser

import scala.reflect.ClassTag

/**
  * Trait that represents classes with isReachable method
  *
  * Currently the only use of it is to avoid the cyclic dependencies between Utils and ParserInput
  */

trait IsReachable[Elem] {
  def apply(index: Int): Elem
  def isReachable(index: Int): Boolean
}

/**
  * Encapsulates all the common operations on each [[Elem]] and [[Repr]] that
  * FastParse needs to perform it's core functionality. This is provided
  * separately, in order to avoid converting every possible input into a
  * lowest-common-denominator type (e.g. `IndexedSeq[Elem]`) to avoid
  * unnecessarily paying conversion-costs and copying the input.
  */
abstract class ReprOps[Elem, Repr] {
  def prettyPrint(input: Repr): String
  def literalize(input: Repr): String
  def errorMessage(input: ParserInput[Elem, Repr], expected: String, idx: Int): String
  def prettyIndex(input: ParserInput[Elem, Repr], index: Int): String

  def slice(value: Repr, start: Int, end: Int): Repr
  def apply(value: Repr, i: Int): Elem
  def length(value: Repr): Int
  def fromArray(input: Array[Elem]): Repr
  def fromSeq(input: Seq[Elem]): Repr
  def fromSingle(input: Elem): Repr
  def toArray(input: Repr): Array[Elem]
  def flatten(input: Seq[Repr]): Repr
}

object ReprOps{
  implicit
  object StringReprOps extends ReprOps[Char, String] {
    def apply(input: String, i: Int) = input.charAt(i)
    def slice(input: String, start: Int, end: Int) = input.slice(start, end)
    def length(input: String) = input.length

    def fromArray(input: Array[Char]): String = input.mkString
    def fromSeq(input: Seq[Char]): String = input.mkString
    def fromSingle(input: Char): String = input.toString
    def toArray(input: String): Array[Char] = input.toCharArray
    def flatten(input: Seq[String]): String = input.mkString
    def prettyPrint(input: String): String = input
    def literalize(input: String): String = Util.literalize(input)
    def errorMessage(input: ParserInput[Char, String], expected: String, idx: Int): String = {
      val locationCode = {
        val first = input.slice(idx - 20, idx)
        val last = input.slice(idx, idx + 20)
        val emptyString = ""
        val lastSnippet: String = last.lines.toSeq.headOption.getOrElse(emptyString)
        val firstSnippet: String = first.reverse.lines.toSeq.headOption.getOrElse(emptyString).reverse

        prettyPrint(firstSnippet) + prettyPrint(lastSnippet) + "\n" + (" " * firstSnippet.length) + "^"
      }
      val literal = literalize(input.slice(idx, idx + 20))
      s"found $literal, expected $expected at index $idx\n$locationCode"
      //TODO Probably we could avoid code duplication by creating only method `locationCode`
      //TODO but it reduces the abstraction
    }

    def prettyIndex(input: ParserInput[Char, String], index: Int): String = {
      input match {
        case IndexedParserInput(data) =>
          var line = 1
          var col = 1
          var i = 0
          while (i < index){
            if (data(i) == '\n') {
              col = 1
              line += 1
            }else{
              col += 1
            }
            i += 1
          }
          s"$line:$col"
        case _ => String.valueOf(index)
      }
    }

  }
}


/**
  * ParserInput class represents data that is needed to parse.
  *
  * It can be regular `IndexedSeq` that behaves as simple array or
  * `Iterator` of `IndexedSeq` batches which is optimized by `dropBuffer` method.
  */
abstract class ParserInput[Elem, Repr] extends IsReachable[Elem] {
  def apply(index: Int): Elem

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
  def slice(from: Int, until: Int): Repr

  def length: Int
  def innerLength: Int

  /**
    * Shows if we can access to the element at given `index`.
    */
  def isReachable(index: Int): Boolean

  val repr: ReprOps[Elem, Repr]

  def checkTraceable(): Unit
}

case class IndexedParserInput[Elem, Repr](data: Repr)
                                         (implicit val repr: ReprOps[Elem, Repr])
  extends ParserInput[Elem, Repr] {
  override def apply(index: Int) = repr.apply(data, index)

  /**
    * As for `IndexedSeq` mode `dropBuffer` does nothing.
    */
  override def dropBuffer(index: Int) = {}

  override def slice(from: Int, until: Int) = repr.slice(data, from, until)

  /**
    * @return Length of internal immutable data. It works equally as `innerLength`.
    */
  override def length: Int = repr.length(data)
  /**
    * @return Length of internal immutable data. It works equally as `length`.
    */
  override def innerLength: Int = length

  /**
    * Simple condition of `index` < `length`
    */
  override def isReachable(index: Int): Boolean = index < length

  def checkTraceable() = ()
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
case class IteratorParserInput[Elem, Repr](data: Iterator[Repr])
                                          (implicit val repr: ReprOps[Elem, Repr],
                                           converter: ReprOps[Elem, Repr],
                                           ct: ClassTag[Elem])
  extends ParserInput[Elem, Repr] {
  private val buffer: UberBuffer[Elem] = new UberBuffer[Elem](16)
  private var firstIdx: Int = 0 // index in the data corresponding to the 0th element in the buffer

  private def requestUntil(until: Int): Boolean = {
    while (this.length <= until && data.hasNext) {
      val chunk = data.next()
      buffer.write(converter.toArray(chunk))
    }
    this.length > until
  }

  /**
    * Requests batches until given `index` and in case of success returns needed element.
    */
  override def apply(index: Int): Elem = {
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
  override def slice(from: Int, until: Int): Repr = {
    requestUntil(until - 1)
    val lo = math.max(from, firstIdx)
    converter.fromArray(buffer.slice(lo - firstIdx, until - firstIdx))
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
    "Cannot perform `.traced` on an `IteratorParserInput`, as it needs to parse " +
      "the input a second time to collect traces, which is impossible after an " +
      "`IteratorParserInput` is used once and the underlying Iterator exhausted."
  )
}