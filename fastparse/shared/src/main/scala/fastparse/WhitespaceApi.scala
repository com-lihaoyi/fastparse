package fastparse

import fastparse.core.Implicits.{Repeater, Sequencer}
import fastparse.all._
import fastparse.core.{ParserApiImpl, Precedence}
import fastparse.parsers.Combinators.Repeat



object WhitespaceApi {

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
        case Mutable.Success(value0, index0, traceParsers0, cut0) =>
          if (index0 > index && cfg.checkForDrop(cut0 | cut)) cfg.input.dropBuffer(index0)

          val oldCapturing = cfg.isCapturing // completely disallow dropBuffer
          cfg.isCapturing = true
          val resWL = WL.parseRec(cfg, index0)
          cfg.isCapturing = oldCapturing

          resWL match {
            case f1: Mutable.Failure => failMore(f1, index, cfg.logDepth)
            case Mutable.Success(value1, index1, traceParsers1, cut1) =>
              p.parseRec(cfg, index1) match {
                case f: Mutable.Failure =>
                  failMore(
                  f, index1, cfg.logDepth,
                  mergeTrace(cfg.traceIndex, traceParsers0, f.traceParsers),
                  cut | cut0
                )
                case Mutable.Success(value2, index2, traceParsers2, cut2) =>
                  val (newIndex, newCut) =
                    if (index2 > index1 || !cfg.input.isReachable(index1)) {
                      if (cfg.checkForDrop(cut | cut0 | cut1 | cut2)) {
                        cfg.input.dropBuffer(index2)
                      }
                      (index2, cut | cut0 | cut1 | cut2)
                    } else {
                      (index0, cut | cut0 | cut2)
                    }

                  success(
                    cfg.success,
                    ev.apply(value0, value2),
                    newIndex,
                    mergeTrace(cfg.traceIndex, traceParsers0, traceParsers2),
                    newCut
                  )
              }
          }
      }
    }

    override def toString = {
      if (!cut && p0 == Pass) p.toString
      else {
        val op = if (cut) "~/" else "~"
        opWrap(p0) + " " + op + " " + opWrap(p)
      }
    }
    override def opPred = Precedence.OtherOp
  }
  def Wrapper(WL: P0) = new Wrapper(WL)
  class Wrapper(WL: P0){
    implicit def parserApi[T, V](p0: T)(implicit c: T => P[V]): WhitespaceApi[V] =
      new WhitespaceApi[V](p0, WL)
  }
}
/**
 * Custom version of `ParserApi`, that behaves the same as the
 * default but injects whitespace in between every pair of tokens. Also
 * provides replacement methods `repX` and `~~` if you wish to call the
 * original un-modified versions of these operators.
 */
class WhitespaceApi[+T](p0: P[T], WL: P0) extends ParserApiImpl[T, Char, String](p0)  {


  def repX[R](implicit ev: Repeater[T, R]): P[R] = Repeat(p0, 0, Int.MaxValue, Pass)

  override def rep[R](implicit ev: Repeater[T, R]): P[R] = Repeat(p0, 0, Int.MaxValue, NoCut(WL))

  def repX[R](min: Int = 0, sep: P[_] = Pass, max: Int = Int.MaxValue)
             (implicit ev: Repeater[T, R]): P[R] = Repeat(p0, min, max, sep)

  override def rep[R](min: Int = 0, sep: P[_] = Pass,
                      max: Int = Int.MaxValue, exactly: Int = -1)
                     (implicit ev: Repeater[T, R]): P[R] = {
    Repeat(p0,
      if (exactly < 0) min else exactly, if (exactly < 0) max else exactly,
      if (sep != Pass) NoCut(WL) ~ sep ~ NoCut(WL) else NoCut(WL))
  }

  def ~~[V, R](p: P[V])
              (implicit ev: Sequencer[T, V, R])
  : P[R] =
    p0 ~ p


  override def ~[V, R](p: P[V])
                      (implicit ev: Sequencer[T, V, R])
  : P[R] = {
    assert(p != null)
    new WhitespaceApi.CustomSequence(WL, if (p0 != WL) p0 else Pass.asInstanceOf[P[T]], p, cut=false)(ev)
  }


  override def ~/[V, R](p: P[V])
                       (implicit ev: Sequencer[T, V, R])
  : P[R] = {
    assert(p != null)
    new WhitespaceApi.CustomSequence(WL, if (p0 != WL) p0 else Pass.asInstanceOf[P[T]], p, cut=true)(ev)
  }

  override def ~/ : P[T] = super.~/

}
