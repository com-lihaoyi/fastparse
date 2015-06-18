package fastparse.parsers

import fastparse._
import fastparse.core.Result._
import fastparse.core.{ParseCtx, Result}

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
        case s: Success.Mutable[_] => success(cfg.success, cfg.input.substring(index, s.index), s.index, s.cut)
        case f: Failure.Mutable => f
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
        case f: Failure.Mutable => failMore(f, index, cfg.trace)
        case s: Result.Success[T] => s
      }
      if (cfg.instrument == null) res
      else cfg.instrument(this, index, () => res)
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
        case s: Success.Mutable[_] => success(cfg.success, (), index, false)
        case f: Failure.Mutable => failMore(f, index, cfg.trace)
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
        case s: Success.Mutable[_] => fail(cfg.failure, s.index)
        case f: Failure.Mutable => success(cfg.success, (), index, false)
      }
      res
    }
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
        case s: Success.Mutable[_] => success(cfg.success, ev.some(s.value), s.index, s.cut)
        case f: Failure.Mutable if f.cut => failMore(f, index, cfg.trace)
        case _ => success(cfg.success, ev.none, index, false)
      }
    }
    override def toString = s"$p.?"
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
      def parseRec(cfg: ParseCtx, index: Int): Result[R] = {
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
        @tailrec def rec(r1: R, rIndex: Int, rCut: Boolean, vIndex: Int): Result[R] = {
          if (vIndex >= ps.length) success(cfg.success, r1, rIndex, rCut)
          else {
            val c = ps(vIndex)
            c.p.parseRec(cfg, rIndex) match {
              case f: Failure.Mutable => failMore(f, rIndex, cfg.trace, cut = c.cut | f.cut | rCut)
              case res2: Success.Mutable[R] => rec(
                c.ev(r1, res2.value), res2.index, c.cut | res2.cut | rCut,
                vIndex + 1
              )
            }
          }
        }
        p0.parseRec(cfg, index) match{
          case f: Failure.Mutable => failMore(f, index, cfg.trace, cut = f.cut)
          case s: Success.Mutable[R] => rec(s.value, s.index, s.cut, 0)
        }
      }
      override def toString = {

        val rhs = for(c <- ps) yield {
          " ~" + (if (c.cut) "!" else "") + " " + c.p
        }
        s"($p0${rhs.mkString})"
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

  case class Cut[T](p: Parser[T]) extends Parser[T]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match{
        case f: Failure.Mutable => failMore(f, index, cfg.trace, false)
        case s: Success.Mutable[T] => success(s, s.value, s.index, true)
      }
    }
    override def toString = p.toString
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
        case f: Failure.Mutable => failMore(f, index, cfg.trace, cut = f.cut)
        case s1: Success.Mutable[_] =>
          val value1 = s1.value
          val cut1 = s1.cut
          //          if (cut) println("CUT! " + this + ":" + s1.index)
          p2.parseRec(cfg, s1.index) match{
            case f: Failure.Mutable => failMore(f, index, cfg.trace, cut = cut | f.cut | cut1)
            case s2: Success.Mutable[_] => success(cfg.success, ev(value1, s2.value), s2.index, s2.cut | cut1 | cut)
          }
      }
    }

    override def toString = {
      def rec(p: Parser[_]): String = p match {
        case p: Sequence[_, _, _] =>
          val op = if(cut || p1.isInstanceOf[Cut[_]]) "~!" else "~"
          rec(p.p1) + " " + op + " " + rec(p.p2)
        case p => p.toString
      }
      "(" + rec(this) + ")"
    }
  }


  /**
   * Repeats the parser over and over. Succeeds with a `Seq` of results
   * if there are more than [[min]] successful parses. uses the [[delimiter]]
   * between parses and discards its results
   */
  case class Repeat[T, +R](p: Parser[T], min: Int, delimiter: Parser[_], until: Parser[_])
                          (implicit ev: Implicits.Repeater[T, R]) extends Parser[R]{

    private[this] val Sentinel = new Object()
    private[this] val FirstStep = if (until != Pass) p | until else p
    private[this] val Step = {
      var step: P[_] = p
      if (delimiter != Pass) step = delimiter ~ p
      if (until != Pass) step = step | until
      step
    }

    def parseRec(cfg: ParseCtx, index: Int) = {
      @tailrec def rec(index: Int,
                       del: Parser[_],
                       lastFailure: Failure.Mutable,
                       acc: ev.Acc,
                       cut: Boolean,
                       count: Int): Result[R] = {
        del.parseRec(cfg, index) match{
          case f1: Failure.Mutable =>
            val cut1 = f1.cut
            if (f1.cut) failMore(f1, index, cfg.trace, true)
            else until.parseRec(cfg, index) match {
              case f: Failure.Mutable =>
                Step.fail(cfg.failure, index, cut1 | f.cut)
              case s: Success[_] => passIfMin(cut, f1, s.index, ev.result(acc), count)
            }
          case s1: Success.Mutable[_] =>
            val cut1 = s1.cut
            val index1 = s1.index
            p.parseRec(cfg, index1) match{
              case f2: Failure.Mutable =>
                val cut2 = f2.cut
                if (cut2 | cut1) failMore(f2, index1, cfg.trace, true)
                else if(del != Pass) passIfMin(cut | s1.cut, f2, index, ev.result(acc), count)
                else until.parseRec(cfg, index1) match {
                  case f: Failure.Mutable =>
                    (if (del == Pass) FirstStep else Step).fail(cfg.failure, index, cut1 | cut2 | f.cut)
                  case s: Success[_] =>
                    passIfMin(cut | s1.cut, f2, s.index, ev.result(acc), count)
                }

              case s2: Success.Mutable[T] =>
                ev.accumulate(s2.value, acc)
                rec(s2.index, delimiter, lastFailure, acc, cut1 | s2.cut, count + 1)
            }
        }
      }

      def passIfMin(cut: Boolean, lastFailure: Failure.Mutable, finalIndex: Int, acc: R, count: Int) = {
        if (count >= min) success(cfg.success, acc, finalIndex, cut)
        else failMore(lastFailure, index, cfg.trace, cut)
      }
      rec(index, Pass, null, ev.initial, false, 0)
    }

    override def toString = {
      if (min == 0 && delimiter == Pass && until == Pass) p + ".rep"
      else{
        val things = Seq(
          if (min == 0) None else Some(min),
          if (delimiter == Pass) None else Some("sep = " + delimiter),
          if (until == Pass) None else Some("end = " + until)
        ).flatten.mkString(", ")
        s"$p.rep($things)"
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
      @tailrec def rec(parserIndex: Int): Result[T] = {
        if (parserIndex >= n) fail(cfg.failure, index)
        else ps0(parserIndex).parseRec(cfg, index) match {
          case s: Success.Mutable[_] => s
          case f: Failure.Mutable if f.cut => failMore(f, index, cfg.trace)
          case _ => rec(parserIndex + 1)
        }
      }
      rec(0)
    }
    override def toString = {
      def rec(p: Parser[_]): String = p match {
        case p: Either[_] => p.ps.map(rec).mkString(" | ")
        case p: Sequence.Flat[_] => p.toString.drop(1).dropRight(1)
        case p => p.toString
      }
      "(" + rec(this) + ")"
    }
  }

}
