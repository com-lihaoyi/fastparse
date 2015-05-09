package fastparse

import scala.annotation.switch
import acyclic.file
import scala.collection.mutable

import scala.language.experimental.macros


object Utils {
  /**
   * Type, which when summoned implicitly, provides the
   * name of the nearest enclosing method for your perusal
   */
  case class FuncName(name: String)
  object FuncName{
    implicit def strToFuncName(s: String) = FuncName(s)

    def impl(c: Compat.Context): c.Expr[FuncName] = {
      import c.universe._

      val sym = Compat.enclosingName(c)
      val simpleName = sym.name.decodedName.toString.trim

      val name = q"$simpleName"

      c.Expr[FuncName](q"fastparse.Utils.FuncName($name)")
    }
  }

  /**
   * Takes a predicate and pre-generates a base64 encoded bit-set, that 
   * evaluates at run-time to create a [[CharBitSet]]. Useful for pre-computing
   * Char predicates that are unfeasible at runtime, e.g. because they're too
   * slow or because they don't work in Scala.js
   */
  def preCompute(pred: Char => Boolean): fastparse.Utils.CharBitSet = macro preComputeImpl

  def preComputeImpl(c: Compat.Context)(pred: c.Expr[Char => Boolean]): c.Expr[CharBitSet] = {
    import c.universe._
    val evaled = c.eval(c.Expr[Char => Boolean](c.resetLocalAttrs(pred.tree.duplicate)))
    val (first, last, array) = CharBitSet.compute((Char.MinValue to Char.MaxValue).filter(evaled))
    val txt = CharBitSet.ints2Hex(array)
    c.Expr[CharBitSet](q"""
      new fastparse.Utils.CharBitSet(fastparse.Utils.CharBitSet.hex2Ints($txt), $first, $last)
    """)
  }
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

  object CharBitSet{
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
    def compute(chars: Seq[Char]) = {
      val first = chars.min
      val last = chars.max
      val span = last - first
      val array = new Array[Int](span / 32 + 1)
      for(c <- chars) array((c - first) >> 5) |= 1 << ((c - first) & 31)
      (first, last, array)
    }
    def apply(chars: Seq[Char]) = {
      val (first, last, array) = compute(chars)
      for(c <- chars) array((c - first) >> 5) |= 1 << ((c - first) & 31)
      new CharBitSet(array, first, last)
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
  final class CharBitSet(array: Array[Int], first: Int, last: Int) extends (Char => Boolean){
    def apply(c: Char) = {
      if (c > last || c < first) false
      else {
        val offset = c - first
        (array(offset >> 5) & 1 << (offset & 31)) != 0
      }
    }
  }

  /**
   * An trie node for quickly matching multiple strings which
   * share the same prefix, one char at a time.
   *
   * The `LongMap` could be pretty easily replaced by a lookup-table
   * `Array[TrieNode]`, but empirically that doesn't seem to give any
   * performance improvements.
   */
  final class TrieNode{
    val children: mutable.LongMap[TrieNode] = mutable.LongMap.empty
    var word: String = null
    def apply(c: Char) = {
      children.getOrNull(c)
    }
  }
}
