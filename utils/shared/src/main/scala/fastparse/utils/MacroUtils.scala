package fastparse.utils

import acyclic.file
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

    val (first, last, array) = Utils.BitSet.compute[Char](
      new Generator[Char]{
        def apply(callback: Generator.Callback[Char]) = {
          (Char.MinValue to Char.MaxValue).filter(evaled).map(callback(_))
        }
      }
    )
    val txt = Utils.HexUtils.ints2Hex(array)
    c.Expr[Utils.BitSet[Char]](q"""
      new fastparse.utils.Utils.BitSet(fastparse.utils.Utils.HexUtils.hex2Ints($txt), $first, $last)
    """)
  }
}
