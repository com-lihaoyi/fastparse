package fastparse.utils

import acyclic.file

import scala.annotation.{switch, tailrec}
import scala.language.experimental.macros

object MacroUtils{
  /**
   * Takes a predicate and pre-generates a base64 encoded bit-set, that
   * evaluates at run-time to create a [[Utils.BitSet]]. Useful for pre-computing
   * Char predicates that are unfeasible at runtime, e.g. because they're too
   * slow or because they don't work in Scala.js
   */
  def preCompute(pred: Char => Boolean): Utils.BitSet[Char] = macro preComputeImpl

  def preComputeImpl(c: Compat.Context)(pred: c.Expr[Char => Boolean]): c.Expr[Utils.BitSet[Char]] = {
    import c.universe._
    val evaled = c.eval(c.Expr[Char => Boolean](c.resetLocalAttrs(pred.tree.duplicate)))
    val (first, last, array) = Utils.BitSet.compute(
      (Char.MinValue to Char.MaxValue).filter(evaled)
    )
    val txt = Utils.HexUtils.ints2Hex(array)
    c.Expr[Utils.BitSet[Char]](q"""
      new fastparse.utils.Utils.BitSet(fastparse.utils.Utils.HexUtils.hex2Ints($txt), $first, $last)
    """)
  }
}
object Utils {

  /**
   * Convert a string to a C&P-able literal. Basically
   * copied verbatim from the uPickle source code.
   */
  def literalize(s: IndexedSeq[Char], unicode: Boolean = true) = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s(i): @switch) match {
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

    sb.result()
  }

  object HexUtils {
    val hexChars = Seq(
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f'
    )
    def hex2Int(hex: String): Int = {
      var res = 0
      for(i <- 0 until hex.length){
        res += hexChars.indexOf(hex(i)) << (4 * (7 - i))
      }
      res
    }
    def hex2Ints(hex: String): Array[Int] = {
      val res = for {
        i <- 0 to hex.length - 1 by 8
      // parseUnsignedInt not implemented in Scala.js
      // java.lang.Long.parseLong also misbehaves
      } yield hex2Int(hex.slice(i, i+8))
      res.toArray
    }

    def ints2Hex(ints: Array[Int]): String = {
      val res = for(int <- ints) yield {
        val s = Integer.toHexString(int)
        "0" * (8-s.length) + s
      }
      res.mkString
    }
  }

  object BitSet {
    def compute[Elem](elems: Seq[Elem])
                     (implicit helper: ElemSetHelper[Elem]) = {

      val first = helper.toInt(elems.min(helper.ordering))
      val last = helper.toInt(elems.max(helper.ordering))
      val span = last - first
      val array = new Array[Int](span / 32 + 1)
      for(c <- elems) array((helper.toInt(c) - first) >> 5) |= 1 << ((helper.toInt(c) - first) & 31)
      (first, last, array)
    }
    def apply[Elem](chars: Seq[Elem])
                   (implicit helper: ElemSetHelper[Elem]) = {
      val (first, last, array) = compute(chars)
      new BitSet[Elem](array, first, last)
    }
  }
  /**
   * A small, fast implementation of a bitset packing up to 65k Chars
   * into 2k Ints (8k Bytes) but using less if the range of inputs
   * is smaller.
   *
   * Empirically seems to be a hell of a lot faster than immutable.Bitset,
   * making the resultant parser up to 2x faster!
   */
  final class BitSet[Elem](array: Array[Int], first: Int, last: Int)
                          (implicit helper: ElemSetHelper[Elem]) extends (Elem => Boolean){
    def apply(c: Elem) = {
      val ci = helper.toInt(c)
      if (ci > last || ci < first) false
      else {
        val offset = ci - first
        (array(offset >> 5) & 1 << (offset & 31)) != 0
      }
    }
  }

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
   * An trie node for quickly matching multiple strings which
   * share the same prefix, one char at a time.
   */
  final class TrieNode[Elem](strings: Seq[IndexedSeq[Elem]], ignoreCase: Boolean = false)
                            (implicit helper: ElemSetHelper[Elem], ordering: Ordering[Elem]) {

    val (min, max, arr) = {
      val ignoreCaseStrings = if (ignoreCase) strings.map(_.map(helper.toLowerCase)) else strings
      val children = ignoreCaseStrings.filter(!_.isEmpty)
                                      .groupBy(_(0))
                                      .map { case (k,ss) => k -> new TrieNode(ss.map(_.tail), ignoreCase) }
      if (children.size == 0) (0, 0, new Array[TrieNode[Elem]](0))
      else {
        val min = helper.toInt(children.keysIterator.min)
        val max = helper.toInt(children.keysIterator.max)
        val arr = new Array[TrieNode[Elem]](max - min + 1)
        for ((k, v) <- children) arr(helper.toInt(k) - min) = v
        (min, max, arr)
      }
    }
    val word: Boolean = strings.exists(_.isEmpty) || arr.isEmpty

    def apply(c: Elem): TrieNode[Elem] = {
      val ci = helper.toInt(c)
      if (ci > max || ci < min) null
      else arr(ci - min)
    }

    /**
     * Returns the length of the matching string, or -1 if not found
     */
    def query(input: IsReachable[Elem], index: Int): Int = {
      @tailrec def rec(offset: Int, currentNode: TrieNode[Elem], currentRes: Int): Int = {
        if (!input.isReachable(index + offset)) currentRes
        else {

          val elem0 = input(index + offset)
          val elem = if(ignoreCase) helper.toLowerCase(elem0) else elem0
          val next = currentNode(elem)
          if (next == null) currentRes
          else rec(
            offset + 1,
            next,
            if (next.word) offset
            else currentRes
          )
        }
      }
      rec(0, this, -1)
    }
  }
}
