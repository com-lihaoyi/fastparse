package fastparse.parsers

import fastparse.Implicits
import fastparse.Utils.FuncName
import Terminals._
import fastparse.core.Result._
import fastparse.core.Mutable
import fastparse.core.{Precedence, ParseCtx, Result, Parser}

import scala.annotation.tailrec
/**
 * Parsers which are made up of other parsers,
 * adding to or combining their behavior
 */
object Combinators {

  /**
   * Captures the string parsed by the given parser [[p]].
   */
  case class Capturing(p: Parser[_]) extends Parser[String]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match {
        case s: Mutable.Success[_] =>
          success(
            cfg.success,
            cfg.input.substring(index, s.index),
            s.index,
            s.traceParsers,
            s.cut
          )
        case f: Mutable.Failure => f
      }
    }
    override def toString = p.toString
  }


  /**
   * Wraps a parser and prints out the indices where it starts
   * and ends, together with its result
   */
  case class Logged[+T](p: Parser[T], msg: String, output: String => Unit) extends Parser[T]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      val indent = "  " * cfg.logDepth
      output(indent + "+" + msg + ":" + index)
      val res = p.parseRec(cfg.copy(logDepth = cfg.logDepth+1), index)
      output(indent + "-" + msg + ":" + index + ":" + res)
      res
    }
  }


  /**
   * A top-level, named parser. Lazily evaluates the wrapped parser
   * [[p]] only when `parse` is called to allow for circular
   * dependencies between parsers.
   */
  case class Rule[+T](name: FuncName, p: () => Parser[T]) extends Parser[T]{
    lazy val pCached = p()
    def parseRec(cfg: ParseCtx, index: Int) = {
      lazy val res  = pCached.parseRec(cfg, index) match{
        case f: Mutable.Failure =>
          println("Rule f " + this + " " + f.traceParsers0)
          failMore(f, index, cfg.trace, f.traceParsers0)
        case s: Mutable.Success[T] => s
      }
      if (cfg.instrument == null) res
      else cfg.instrument(this, index, () => res.toResult)
      res
    }
    override def toString = name.name
    override def shortTraced = true
  }

  /**
   * Wraps another parser, succeeding/failing identically
   * but consuming no input
   */
  case class Lookahead(p: Parser[_]) extends Parser[Unit]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match{
        case s: Mutable.Success[_] => success(cfg.success, (), index, Nil, false)
        case f: Mutable.Failure => failMore(f, index, cfg.trace, f.traceParsers)
      }
    }
    override def toString = s"&($p)"
  }
  /**
   * Wraps another parser, succeeding it it fails and failing
   * if it succeeds. Neither case consumes any input
   */
  case class Not(p: Parser[_]) extends Parser[Unit]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      val res0 = p.parseRec(cfg, index)
      val res = res0 match{
        case s: Mutable.Success[_] => fail(cfg.failure, s.index, cfg.trace)
        case f: Mutable.Failure => success(cfg.success, (), index, Nil, false)
      }
      res
    }
    override def opPred = Precedence.PrefixOp
    override def toString = s"!($p)"
  }


  /**
   * Wraps a parser and succeeds with `Some` if [[p]] succeeds,
   * and succeeds with `None` if [[p]] fails.
   */
  case class Optional[+T, R](p: Parser[T])
                            (implicit ev: Implicits.Optioner[T, R]) extends Parser[R]{

    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match{
        case s: Mutable.Success[_] => success(cfg.success, ev.some(s.value), s.index, s.traceParsers, s.cut)
        case f: Mutable.Failure if f.cut => failMore(f, index, cfg.trace, f.traceParsers)
        case _ => success(cfg.success, ev.none, index, Nil, false)
      }
    }
    override def toString = s"${opWrap(p)}.?"
  }


  /**
   * Contains an optimized version of [[Sequence]] called [[Sequence.Flat]] that
   * combines a tree of [[Sequence]] nodes from the left, into a single
   * tail-recursive function working over a `Vector` of their contents.
   *
   * Intentionally completely type-unsafe internally, using casting all
   * over the place, because it's near impossible to make the variable-length
   * heterogenous-typed list type-safe without going crazy. If constructed by
   * `flatten`-ing out a [[Sequence]], the types are checked when the [[Sequence]]
   * was constructed, so it's still safe.
   */
  object Sequence{

    /**
     * The contents of a [[Sequence]] node, minus the left subtree.
     */
    case class Chain[R](p: Parser[R], cut: Boolean)(val ev: Implicits.Sequencer[R, R, R])
    case class Flat[R](p0: Parser[R],
                       ps: Vector[Chain[R]]) extends Parser[R] {
      def parseRec(cfg: ParseCtx, index: Int): Mutable[R] = {
        /**
         * Given
         *
         * A ~ B ~ C ~ D
         *
         * Perform the following iterations:
         *
         * rB = evB(pA, pB)
         * rC = evC(rB, pC)
         * rD = evD(rC, pD)
         * return rD
         */
        @tailrec def rec(r1: R,
                         rIndex: Int,
                         rCut: Boolean,
                         vIndex: Int,
                         traceParsers: List[Parser[_]]): Mutable[R] = {
          println("rec " + traceParsers)
          if (vIndex >= ps.length) success(cfg.success, r1, rIndex, traceParsers, rCut)
          else {
            val c = ps(vIndex)
            c.p.parseRec(cfg, rIndex) match {
              case f: Mutable.Failure => failMore(
                f,
                rIndex,
                cfg.trace,
                traceParsers = f.traceParsers0 ::: traceParsers,
                cut = c.cut | f.cut | rCut
              )
              case res2: Mutable.Success[R] => rec(
                c.ev(r1, res2.value),
                res2.index,
                c.cut | res2.cut | rCut,
                vIndex + 1,
                res2.traceParsers ::: traceParsers
              )
            }
          }
        }
        p0.parseRec(cfg, index) match{
          case f: Mutable.Failure =>
            failMore(f, index, cfg.trace, traceParsers = f.traceParsers0, cut = f.cut)
          case s: Mutable.Success[R] => rec(s.value, s.index, s.cut, 0, s.traceParsers)
        }
      }
      override def opPred = Precedence.OtherOp
      override def toString = {

        val rhs = for(c <- ps) yield {
          " ~" + (if (c.cut) "!" else "") + " " + opWrap(c.p)
        }
        s"${opWrap(p0)}${rhs.mkString}"
      }
    }

    /**
     * The types here are all lies. It's ok, just trust the
     * code to do the right thing!
     *
     *
     * A ~ B ~ C ~ D
     * ((A ~ B) ~ C) ~ D
     */
    def flatten[R](s: Sequence[R, R, R]): Flat[R] = {
      def rec(s: Sequence[R, R, R]): Flat[R] = {
        val ev2 = s.ev2.asInstanceOf[Implicits.Sequencer[R, R, R]]
        s.p1 match{
          case f: Flat[R] =>
            f.copy(ps = f.ps :+ Chain[R](s.p2, s.cut)(ev2))
          case p: Sequence[R, R, R] =>
            val res = rec(p)
            res.copy(ps = res.ps :+ Chain[R](s.p2, s.cut)(ev2))
          case p => Flat(p, Vector(Chain[R](s.p2, s.cut)(ev2)))
        }
      }
      rec(s)
    }
  }

  /**
   * Parsers two things in a row, returning a tuple of the two
   * results if both things succeed
   */
  case class Sequence[+T1, +T2, R](p1: Parser[T1], p2: Parser[T2], cut: Boolean)
                                  (implicit ev: Implicits.Sequencer[T1, T2, R]) extends Parser[R]{
    def ev2: Implicits.Sequencer[_, _, _] = ev
    def parseRec(cfg: ParseCtx, index: Int) = {
      p1.parseRec(cfg, index) match{
        case f: Mutable.Failure => failMore(f, index, cfg.trace, traceParsers = List(p1), cut = f.cut)
        case s1: Mutable.Success[_] =>
          val traceParsers1 = s1.traceParsers
          val value1 = s1.value
          val cut1 = s1.cut
          //          if (cut) println("CUT! " + this + ":" + s1.index)
          p2.parseRec(cfg, s1.index) match{
            case f: Mutable.Failure => failMore(
              f,
              index,
              cfg.trace,
              traceParsers = traceParsers1 ::: f.traceParsers0,
              cut = cut | f.cut | cut1
            )
            case s2: Mutable.Success[_] =>
              success(cfg.success, ev(value1, s2.value), s2.index, s2.traceParsers ::: traceParsers1, s2.cut | cut1 | cut)
          }
      }
    }
    override def opPred = Precedence.OtherOp
    override def toString = {
      val op = if(cut) "~!" else "~"
      opWrap(p1) + " " + op + " " + opWrap(p2)
    }
  }


  case class Cut[T](p: Parser[T]) extends Parser[T]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match{
        case f: Mutable.Failure => failMore(f, index, cfg.trace, f.traceParsers, cut = false)
        case s: Mutable.Success[T] => success(s, s.value, s.index, null, cut = true)
      }
    }
    override def opPred = Precedence.OtherOp
    override def toString = p.toString
  }
  /**
   * Repeats the parser over and over. Succeeds with a `Seq` of results
   * if there are more than [[min]] successful parses. uses the [[delimiter]]
   * between parses and discards its results
   */
  case class Repeat[T, +R](p: Parser[T], min: Int, delimiter: Parser[_])
                          (implicit ev: Implicits.Repeater[T, R]) extends Parser[R]{


    def parseRec(cfg: ParseCtx, index: Int) = {
      @tailrec def rec(index: Int,
                       del: Parser[_],
                       lastFailure: Mutable.Failure,
                       acc: ev.Acc,
                       cut: Boolean,
                       count: Int): Mutable[R] = {
        del.parseRec(cfg, index) match{
          case f1: Mutable.Failure =>
            val cut1 = f1.cut
            if (f1.cut) failMore(f1, index, cfg.trace, f1.traceParsers0, cut = true)
            else passIfMin(cut, f1, index, ev.result(acc), count)

          case s1: Mutable.Success[_] =>
            val cut1 = s1.cut
            val index1 = s1.index
            p.parseRec(cfg, index1) match{
              case f2: Mutable.Failure =>
                val cut2 = f2.cut
                if (cut2 | cut1) failMore(f2, index1, cfg.trace, f2.traceParsers0, cut = true)
                else passIfMin(cut | s1.cut, f2, index, ev.result(acc), count)

              case s2: Mutable.Success[T] =>
                ev.accumulate(s2.value, acc)
                rec(s2.index, delimiter, lastFailure, acc, cut1 | s2.cut, count + 1)
            }
        }
      }

      def passIfMin(cut: Boolean,
                    lastFailure: Mutable.Failure,
                    finalIndex: Int,
                    acc: R,
                    count: Int) = {
        if (count >= min) success(cfg.success, acc, finalIndex, lastFailure.traceParsers0, cut)
        else  failMore(lastFailure, index, cfg.trace, lastFailure.traceParsers0, cut = cut)

      }
      rec(index, Pass, null, ev.initial, false, 0)
    }
    override def toString = {
      if (min == 0 && delimiter == Pass) opWrap(p) + ".rep"
      else{
        val things = Seq(
          if (min == 0) None else Some(min),
          if (delimiter == Pass) None else Some("sep = " + delimiter)
        ).flatten.mkString(", ")
        s"${opWrap(p)}.rep($things)"
      }
    }
  }

  object Either{
    def flatten[T](p: Vector[Parser[T]]): Vector[Parser[T]] = p.flatMap{
      case Either(ps@_*) => ps
      case p => Vector(p)
    }
  }
  /**
   * Parses using one parser or the other, if the first one fails. Returns
   * the first one that succeeds and fails if both fail
   */
  case class Either[T](ps: Parser[T]*) extends Parser[T]{
    private[this] val ps0 = ps.toArray
    private[this] val n = ps0.length
    def parseRec(cfg: ParseCtx, index: Int) = {
      @tailrec def rec(parserIndex: Int, traceParsers: List[Parser[_]]): Mutable[T] = {
        if (parserIndex >= n) fail(cfg.failure, index, cfg.trace)
        else ps0(parserIndex).parseRec(cfg, index) match {
          case s: Mutable.Success[_] =>
            s.traceParsers :::= traceParsers
            s
          case f: Mutable.Failure if f.cut => failMore(f, index, cfg.trace, f.traceParsers)
          case f: Mutable.Failure => rec(parserIndex + 1, f.traceParsers0 ::: traceParsers)
        }
      }
      rec(0, Nil)
    }
    override def opPred = if (ps.length == 1) ps(0).opPred else Precedence.|
    override def toString = {
      ps.map(opWrap).mkString(" | ")
    }
  }

}
