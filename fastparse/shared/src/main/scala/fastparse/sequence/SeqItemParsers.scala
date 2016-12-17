package fastparse.sequence

import fastparse.utils.Utils._
import fastparse.core.{ ParseCtx, Parser }
import fastparse.utils.ReprOps

object SeqItemParsers {
  /**
   * Leaf parsers which do not contain any other parsers, and do simple things
   */
  object Terminals {
    /**
     * A parser that succeeds if the PartialFunction is defined for this item
     * and returns result of the PartialFunction
     */
    case class PartialFunctionRule[+T, Elem, Repr](name: String, pf: PartialFunction[Elem, T])(implicit repr: ReprOps[Elem, Repr])
        extends Parser[T, Elem, Repr] {
      override def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
        if (!cfg.input.isReachable(index)) failMore(cfg.failure, index, cfg.logDepth)
        else {
          val e = cfg.input(index)
          if (cfg.instrument == null) {
            if (pf.isDefinedAt(e)) success(cfg.success, pf(e), index + 1, Set.empty, false)
            else failMore(cfg.failure, index, cfg.logDepth)
          } else {
            lazy val res = if (pf.isDefinedAt(e)) success(cfg.success, pf(e), index + 1, Set.empty, false) else failMore(cfg.failure, index, cfg.logDepth)
            cfg.instrument(this, index, () => res.toResult)
            res
          }
        }
      }
      override def toString = name
      override def shortTraced = true
    }
  }

  /**
   * Much faster that p1 | p2 for single item that must match somes values
   */
  object Intrinsics {
    /**
     * Parses a single item if it passes the predicate
     */
    case class ElemPred[Elem, Repr](
      name: String,
      predicate: Elem => Boolean
    )(implicit repr: ReprOps[Elem, Repr])
        extends Parser[Unit, Elem, Repr] {
      override def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
        val input = cfg.input
        if (!input.isReachable(index)) fail(cfg.failure, index)
        else if (predicate(input(index))) success(cfg.success, (), index + 1, Set.empty, false)
        else fail(cfg.failure, index)
      }

      override def toString = s"$name($predicate)"
    }

    /**
     * Parses a single item if its contained in the lists of allowed items
     */
    case class ElemIn[Elem, Repr](
      name: String,
      items: Seq[Elem]*
    )(implicit repr: ReprOps[Elem, Repr])
        extends Parser[Unit, Elem, Repr] {
      override def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
        val input = cfg.input
        if (!input.isReachable(index)) fail(cfg.failure, index)
        else if (items.contains(input(index))) success(cfg.success, (), index + 1, Set.empty, false)
        else fail(cfg.failure, index)
      }

      override def toString = s"$name(${repr.literalize(repr.flatten(items.map(repr.fromSeq)))})"
    }

    /**
     * Keeps consuming items until the predicate [[pred]] become false.
     * Functionnaly equivalent to using `.rep` and [[ElemPred]], but much faster.
     */
    case class ElemsWhile[Elem, Repr](
      name: String,
      pred: Elem => Boolean,
      min: Int = 1
    )(implicit repr: ReprOps[Elem, Repr])
        extends Parser[Unit, Elem, Repr] {
      override def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
        var curr = index
        val input = cfg.input
        while (input.isReachable(curr) && pred(input(curr))) curr += 1
        if (curr - index < min) fail(cfg.failure, curr)
        else success(cfg.success, (), curr, Set.empty, false)
      }

      override def toString = s"$name($pred)"
    }
  }
}