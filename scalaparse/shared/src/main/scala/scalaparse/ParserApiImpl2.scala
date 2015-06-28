package scalaparse

import fastparse.Implicits.{Repeater, Sequencer}
import fastparse.ParserApiImpl
import fastparse.core._
import fastparse.all._
import fastparse.parsers.Combinators.Repeat

import scala.Mutable

object ParserApiImpl2 {

  /**
   * Custom whitespace-aware semicolon-inference-friendly version of
   * [[fastparse.parsers.Combinators.Sequence]]. Consumes whitespace
   * between the LHS [[p0]] and RHS [[p]] parsers.
   *
   * If the whitespace succeeds but the parser on the right succeeds
   * *with no input*, then backtrack to the start of the whitespace
   * and ignore any `cut`s that came from it. This it meant to avoid
   * capturing trailing whitespace, which needs to be present for
   * semi-colon inference to work properly
   */
  case class CustomSequence[+T, +R, +V](WL: P0, p0: P[T], p: P[V], cut: Boolean)
                                       (implicit ev: Sequencer[T, V, R]) extends P[R] {
    def parseRec(cfg: ParseCtx, index: Int) = {
      p0.parseRec(cfg, index) match {
        case f: Mutable.Failure => failMore(f, index, cfg.logDepth, f.traceParsers, false)
        case s: Mutable.Success[T] =>
          val index0 = s.index
          val cut0 = s.cut
          val traceParsers0 = s.traceParsers
          WL.parseRec(cfg, s.index) match {
            case f1: Mutable.Failure => failMore(f1, index, cfg.logDepth)
            case s1: Mutable.Success[Unit] =>
              val index1 = s1.index
              val cut1 = s1.cut
              p.parseRec(cfg, s1.index) match {
                case f: Mutable.Failure => failMore(f, s.index, cfg.logDepth, traceParsers0 ::: f.traceParsers, cut | cut0)
                case s2: Mutable.Success[V] =>
                  val index2 = s2.index
                  val cut2 = s2.cut
                  val traceParsers2 = s2.traceParsers
                  val (newIndex, newCut) =
                    if (index2 > index1 || index1 == cfg.input.length) (index2, cut | cut0 | cut1 | cut2)
                    else (index0, cut | cut0 | cut2)

                  success(
                    s,
                    ev.apply(s.value, s2.value),
                    newIndex,
                    traceParsers0 ::: traceParsers2,
                    newCut
                  )
              }
          }
      }
    }

    override def toString = {
      if (!cut && p0 == Pass) p.toString
      else {
        val op = if (cut) "~!" else "~"
        opWrap(p0) + " " + op + " " + opWrap(p)
      }
    }
    override def opPred = Precedence.OtherOp
  }

}
/**
 * Custom version of `ParserApi`, that behaves the same as the
 * default but injects whitespace in between every pair of tokens. Also
 * provides replacement methods `repX` and `~~` if you wish to call the
 * original un-modified versions of these operators.
 */
class ParserApiImpl2[+T](p0: P[T], WL: P0) extends ParserApiImpl(p0)  {


  def repX[R](implicit ev: Repeater[T, R]): P[R] = Repeat(p0, 0, Pass)

  override def rep[R](implicit ev: Repeater[T, R]): P[R] = Repeat(p0, 0, NoCut(WL))

  def repX[R](min: Int = 0, sep: P[_] = Pass)
             (implicit ev: Repeater[T, R]): P[R] = Repeat(p0, min, sep)

  override def rep[R](min: Int = 0, sep: P[_] = Pass)
                     (implicit ev: Repeater[T, R]): P[R] = {
    Repeat(p0, min, if (sep != Pass) NoCut(WL) ~ sep ~ NoCut(WL) else NoCut(WL))
  }

  def ~~[V, R](p: P[V])
              (implicit ev: Sequencer[T, V, R])
  : P[R] =
    p0 ~ p


  override def ~[V, R](p: P[V])
                      (implicit ev: Sequencer[T, V, R])
  : P[R] = {
    assert(p != null)
    new ParserApiImpl2.CustomSequence(WL, if (p0 != WL) p0 else Pass.asInstanceOf[P[T]], p, cut=false)(ev)
  }


  override def ~![V, R](p: P[V])
                       (implicit ev: Sequencer[T, V, R])
  : P[R] = {
    assert(p != null)
    new ParserApiImpl2.CustomSequence(WL, if (p0 != WL) p0 else Pass.asInstanceOf[P[T]], p, cut=true)(ev)
  }

}
