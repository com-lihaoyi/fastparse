package fastparse.parsers
import acyclic.file
import fastparse.Implicits
import fastparse.Utils.FuncName
import Terminals._
import fastparse.core.Parsed._
import fastparse.core.Mutable
import fastparse.core.{Precedence, ParseCtx, Parsed, Parser}

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
        case Mutable.Success(value0, index0, traceParsers0, cut0) =>
          success(
            cfg.success,
            cfg.input.substring(index, index0),
            index0,
            traceParsers0,
            cut0
          )
        case f: Mutable.Failure => f
      }
    }
    override def toString = p.toString
  }

  /**
   * Wrap a parser in this if you don't want for it to show up in a stack trace
   */
  case class NoTrace[T](p: Parser[T]) extends Parser[T]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match {
        case s: Mutable.Success[_] =>
          s.traceParsers = Nil
          s
        case f: Mutable.Failure =>
          f.traceParsers = Nil
          f
      }
    }
    override def toString = p.toString
  }

  /**
   * A wrapper that replaces target parser and its inner parsers in the stack trace.
   * Useful for providing more high-level error messages without going into details.
   * For example, "expected CharPred(...)..." can become "expected IDENT...".
   *
   * @param msg The message for the wrapper
   */
  case class Opaque[+T](p: Parser[T], msg: String) extends Parser[T]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match {
        case s: Mutable.Success[_] =>
          s.traceParsers = List(this)
          s
        case f: Mutable.Failure =>
          f.index = index
          f.lastParser = this
          f.traceParsers = List(this)
          f
      }
    }
    override def toString = msg
  }

  /**
   *
   */
  case class NoCut[T](p: Parser[T]) extends Parser[T]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match {
        case s: Mutable.Success[T] =>
          s.cut = false
          s
        case f: Mutable.Failure =>
          f.cut = false
          f
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
      if (cfg.logDepth == -1) p.parseRec(cfg, index)
      else {
        val indent = "  " * cfg.logDepth
        output(s"$indent+$msg:$index")
        val depth = cfg.logDepth
        cfg.logDepth += 1
        val res = p.parseRec(cfg, index)
        cfg.logDepth = depth
        val strRes = res match{
          case s: Mutable.Success[T] => s"Success(${s.index}${if (s.cut) ", cut" else ""})"
          case f: Mutable.Failure =>
            val stack = Failure.filterFullStack(f.fullStack)
            val trace = Failure.formatStackTrace(
              stack,
              f.input,
              index,
              Failure.formatParser(f.lastParser, f.input, f.index)
            )
            s"Failure($trace${if (f.cut) ", cut" else ""})"
        }
        output(s"$indent-$msg:$index:$strRes")
        res
      }
    }
  }


  /**
   * A top-level, named parser. Lazily evaluates the wrapped parser
   * [[p]] only when `parse` is called to allow for circular
   * dependencies between parsers.
   */
  case class Rule[+T](name: FuncName, p: () => Parser[T]) extends Parser[T]{
    private[this] lazy val pCached = p()

    def parseRec(cfg: ParseCtx, index: Int) = {

      if (cfg.instrument == null) {
        pCached.parseRec(cfg, index) match{
          case f: Mutable.Failure => failMore(f, index, cfg.logDepth)
          case s: Mutable.Success[T] => s
        }
      } else {
        lazy val res = pCached.parseRec(cfg, index) match{
          case f: Mutable.Failure => failMore(f, index, cfg.logDepth)
          case s: Mutable.Success[T] => s
        }
        cfg.instrument(this, index, () => res.toResult)
        res
      }
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
        case s: Mutable.Success[_] =>
          s.cut = false
          success(cfg.success, (), index, s.traceParsers, false)
        case f: Mutable.Failure =>
          f.cut = false
          failMore(f, index, cfg.logDepth)
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
        case s: Mutable.Success[_] => fail(cfg.failure, s.index)
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
        case f: Mutable.Failure if f.cut => failMore(f, index, cfg.logDepth)
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
          if (vIndex >= ps.length) success(cfg.success, r1, rIndex, traceParsers, rCut)
          else {
            val c = ps(vIndex)
            c.p.parseRec(cfg, rIndex) match {
              case f: Mutable.Failure => failMore(
                f,
                rIndex,
                cfg.logDepth,
                traceParsers = f.traceParsers ::: traceParsers,
                cut = c.cut | f.cut | rCut
              )
              case Mutable.Success(value0, index0, traceParsers0, cut0)  => rec(
                c.ev(r1, value0),
                index0,
                c.cut | cut0 | rCut,
                vIndex + 1,
                traceParsers0 ::: traceParsers
              )
            }
          }
        }
        p0.parseRec(cfg, index) match{
          case f: Mutable.Failure => failMore(f, index, cfg.logDepth, cut = f.cut)
          case Mutable.Success(value0, index0, traceParsers0, cut0) =>
            rec(value0, index0, cut0, 0, traceParsers0)
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
        case f: Mutable.Failure => failMore(f, index, cfg.logDepth, traceParsers = if(cfg.traceIndex == -1) Nil else List(p1), cut = f.cut)
        case Mutable.Success(value0, index0, traceParsers0, cut0)  =>
          //          if (cut) println("CUT! " + this + ":" + s1.index)
          p2.parseRec(cfg, index0) match{
            case f: Mutable.Failure => failMore(
              f,
              index,
              cfg.logDepth,
              traceParsers = traceParsers0 ::: f.traceParsers,
              cut = cut | f.cut | cut0
            )
            case Mutable.Success(value1, index1, traceParsers1, cut1)  =>
              success(cfg.success, ev(value0, value1), index1, traceParsers1 ::: traceParsers0, cut1 | cut0 | cut)
          }
      }
    }
    override def opPred = Precedence.OtherOp
    override def toString = {
      val op = if(cut) "~/" else "~"
      opWrap(p1) + " " + op + " " + opWrap(p2)
    }
  }


  case class Cut[T](p: Parser[T]) extends Parser[T]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match{
        case f: Mutable.Failure => failMore(f, index, cfg.logDepth, cut = false)
        case s: Mutable.Success[T] => success(s, s.value, s.index, s.traceParsers, cut = true)
      }
    }
    override def opPred = Precedence.OtherOp
    override def toString = p.toString
  }
  /**
   * Repeats the parser over and over. Succeeds with a `Seq` of results
   * if there are more than [[min]] and less than [[max]] successful parses.
   * The range [[min]] and [[max]] bounds are inclusive.
   * It uses the [[delimiter]] parser between parses and discards its results.
   */
  case class Repeat[T, +R](p: Parser[T], min: Int, max: Int, delimiter: Parser[_])
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
            if (cut1) failMore(f1, index, cfg.logDepth, cut = true)
            else passInRange(cut, f1, index, ev.result(acc), count)

          case Mutable.Success(value0, index0, traceParsers0, cut0)  =>
            p.parseRec(cfg, index0) match{
              case f2: Mutable.Failure =>
                val cut2 = f2.cut
                if (cut2 | cut0) failMore(f2, index0, cfg.logDepth, cut = true)
                else passInRange(cut | cut0, f2, index, ev.result(acc), count)

              case Mutable.Success(value1, index1, traceParsers1, cut1)  =>
                ev.accumulate(value1, acc)
                val counted = count + 1
                if (counted < max)
                  rec(index1, delimiter, lastFailure, acc, cut0 | cut1, counted)
                else
                  passInRange(cut0 | cut1, lastFailure, index1, ev.result(acc), counted)
            }
        }
      }

      def passInRange(cut: Boolean,
                    lastFailure: Mutable.Failure,
                    finalIndex: Int,
                    acc: R,
                    count: Int) = {
        if (min <= count) {
          val parsers =
            if (null == lastFailure)
              List.empty[Parser[_]]
            else
              lastFailure.traceParsers
          success(cfg.success, acc, finalIndex, parsers, cut)
        } else failMore(lastFailure, index, cfg.logDepth, cut = cut)
      }

      // don't call the parseRec at all, if max is "0", as our parser corresponds to `Pass` in that case.
      if (max == 0 ) {
        success(cfg.success, ev.result(ev.initial), index, List.empty[Parser[_]], false)
      } else {
        rec(index, Pass, null, ev.initial, false, 0)
      }
    }
    override def toString = {
      val things = Seq(
        if (min == 0) None else Some(min),
        if (delimiter == Pass) None else Some("sep = " + delimiter),
        if (max == Int.MaxValue) None else Some("max = " + max)
      ).flatten.mkString(", ")
      if (things.isEmpty) opWrap(p) + ".rep"
      else s"${opWrap(p)}.rep($things)"
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
        if (parserIndex >= n) fail(cfg.failure, index)
        else ps0(parserIndex).parseRec(cfg, index) match {
          case s: Mutable.Success[T] =>
            s.traceParsers :::= traceParsers
            s
          case f: Mutable.Failure if f.cut => failMore(f, index, cfg.logDepth)
          case f: Mutable.Failure => rec(parserIndex + 1, f.traceParsers ::: traceParsers)
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
