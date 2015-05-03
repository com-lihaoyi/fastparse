package parsing

import scala.annotation.switch
import acyclic.file

object Utils {
  /**
   * Convert a string to a C&P-able literal. Basically
   * copied verbatim from the uPickle source code.
   */
  def literalize(s: String, unicode: Boolean = true) = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"')

  }


  /**
   * A small, fast implementation of a bitset packing up to 65k Chars
   * into 2k Ints (8k Bytes) but using less if the range of inputs
   * is smaller.
   *
   * Empirically seems to be a hell of a lot faster than immutable.Bitset,
   * making the resultant parser up to 2x faster!
   */
  class CharBitSet(chars: Seq[Char]){
    private[this] val first = chars.min
    private[this] val last = chars.max
    private[this] val span = last - first
    private[this] val array = new Array[Int](span / 32 + 1)
    for(c <- chars) array((c - first) >> 5) |= 1 << ((c - first) & 31)


    def apply(c: Char) = {
      if (c > last || c < first) false
      else {
        val offset = c - first
        (array(offset >> 5) & 1 << (offset & 31)) != 0
      }
    }
  }
}
