package scalaparse

import fastparse.Implicits.{Repeater, Sequencer}
import fastparse.ParserApiImpl
import fastparse.core.{Precedence, ParseCtx}
import fastparse.core.Result
import fastparse.all._
import fastparse.parsers.Combinators.Repeat

object ParserApiImpl2 {


  case class CustomSequence[+T, +R, +V](WL: P0, p0: P[T], p: P[V], cut: Boolean)(implicit ev: Sequencer[T, V, R]) extends P[R] {
    def parseRec(cfg: ParseCtx, index: Int) = {
      p0.parseRec(cfg, index) match {
        case f: Result.Failure.Mutable => failMore(f, index, cfg.trace, false)
        case s: Result.Success.Mutable[T] =>
          val index0 = s.index
          val cut0 = s.cut
          WL.parseRec(cfg, s.index) match {
            case s1: Result.Success[Unit] =>
              val index1 = s1.index
              p.parseRec(cfg, s1.index) match {
                case f: Result.Failure.Mutable => failMore(f, s.index, cfg.trace, cut | cut0)
                case s2: Result.Success.Mutable[V] =>
                  val index2 = s2.index
                  val cut2 = s2.cut
                  val newIndex = if (index2 > index1 || index1 == cfg.input.length) index2 else index0
                  success(
                    s,
                    ev.apply(s.value, s2.value),
                    newIndex,
                    cut | cut0 | cut2
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
 * default but injects whitespace in between every pair of tokens
 */
class ParserApiImpl2[+T](p0: P[T], WL: P0) extends ParserApiImpl(p0)  {


  def repX[R](implicit ev: Repeater[T, R]): P[R] = Repeat(p0, 0, Pass)

  override def rep[R](implicit ev: Repeater[T, R]): P[R] = Repeat(p0, 0, WL)

  def repX[R](min: Int = 0, sep: P[_] = Pass)
             (implicit ev: Repeater[T, R]): P[R] = Repeat(p0, min, sep)

  override def rep[R](min: Int = 0, sep: P[_] = Pass)
                     (implicit ev: Repeater[T, R]): P[R] = Repeat(p0, min, WL ~ sep ~ WL)

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
