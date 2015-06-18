package fastparse

import fastparse.Utils.CharBitSet

import scala.language.experimental.macros

/**
 * Created by nmrp3 on 18/06/15.
 */
object MacroUtils {

  def funcNameImpl(c: Compat.Context): c.Expr[FuncName] = {
    import c.universe._

    val sym = Compat.enclosingName(c)
    val simpleName = sym.name.decoded.toString.trim
    val fullName = sym.fullName.trim

    val name = q"$simpleName"

    c.Expr[FuncName](q"fastparse.FuncName($name, $fullName)")
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

}
