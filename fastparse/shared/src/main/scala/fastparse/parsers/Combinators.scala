package fastparse.parsers
import acyclic.file
import fastparse.utils.ReprOps
import fastparse.core.Implicits
import Terminals._
import fastparse.core.Parsed._
import fastparse.core.Mutable
import fastparse.core.{ParseCtx, Parsed, Parser, Precedence}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
/**
 * Parsers which are made up of other parsers,
 * adding to or combining their behavior
 */
object Combinators {

  /**
   * Captures the string parsed by the given parser [[p]].
   */
  case class Capturing[Elem, Repr](p: Parser[_, Elem, Repr])
                                  (implicit repr: ReprOps[Elem, Repr])
      extends Parser[Repr, Elem, Repr] {
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val oldCapturing = cfg.isCapturing
      cfg.isCapturing = true
      val res = p.parseRec(cfg, index)
      cfg.isCapturing = oldCapturing

      res match {
        case Mutable.Success(value0, index0, traceParsers0, cut0) =>
          success(
            cfg.success,
            cfg.input.slice(index, index0),
            index0,
            traceParsers0,
            cut0
          )
        case f: Mutable.Failure[Elem, Repr] => f
      }
    }
    override def toString = p.toString
  }

  /**
   * Wrap a parser in this if you don't want for it to show up in a stack trace
   */
  case class NoTrace[T, Elem, Repr](p: Parser[T, Elem, Repr])
                                   (implicit repr: ReprOps[Elem, Repr]) extends Parser[T, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      p.parseRec(cfg, index) match {
        case s: Mutable.Success[_, Elem, Repr] =>
          s.traceParsers = Set.empty
          s
        case f: Mutable.Failure[Elem, Repr] =>
          f.traceParsers = Set.empty
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
  case class Opaque[+T, Elem, Repr](p: Parser[T, Elem, Repr], msg: String)
                                   (implicit repr: ReprOps[Elem, Repr])
    extends Parser[T, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      p.parseRec(cfg, index) match {
        case s: Mutable.Success[_, Elem, Repr] =>
          if (cfg.traceIndex != -1) s.traceParsers = Set(this)
          s
        case f: Mutable.Failure[Elem, Repr] =>
          f.index = index
          f.lastParser = this
          if (cfg.traceIndex != -1) f.traceParsers = Set(this)
          f
      }
    }
    override def toString = msg
  }

  /**
   *
   */
  case class NoCut[T, Elem, Repr](p: Parser[T, Elem, Repr])
                                 (implicit repr: ReprOps[Elem, Repr]) extends Parser[T, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val oldNoCut = cfg.isNoCut
      cfg.isNoCut = true
      val res = p.parseRec(cfg, index)
      cfg.isNoCut = oldNoCut

      res match {
        case s: Mutable.Success[T, Elem, Repr] =>
          s.cut = false
          s
        case f: Mutable.Failure[Elem, Repr] =>
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
  case class Logged[+T, Elem, Repr](p: Parser[T, Elem, Repr],
                                    msg: String, output: String => Unit)
                                   (implicit repr: ReprOps[Elem, Repr]) extends Parser[T, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      if (cfg.logDepth == -1 || cfg.traceIndex != -1) p.parseRec(cfg, index)
      else {
        val indent = "  " * cfg.logDepth
        output(s"$indent+$msg:${repr.prettyIndex(cfg.input, index)}")
        val depth = cfg.logDepth
        cfg.logDepth += 1
        val res = p.parseRec(cfg, index)
        cfg.logDepth = depth
        val strRes = res match{
          case s: Mutable.Success[T, Elem, Repr] =>
            s"Success(${repr.prettyIndex(cfg.input, s.index)}${if (s.cut) ", cut" else ""})"
          case f: Mutable.Failure[Elem, Repr] =>
            val stack = Failure.filterFullStack(f.fullStack)
            val trace = Failure.formatStackTrace(
              stack.reverse,
              f.input,
              index,
              Failure.formatParser(f.lastParser, f.input, f.index)
            )
            s"Failure($trace${if (f.cut) ", cut" else ""})"
        }
        output(s"$indent-$msg:${repr.prettyIndex(cfg.input, index)}:$strRes")
        res
      }
    }
    override def toString = p.toString + ".log()"
  }


  /**
   * A top-level, named parser. Lazily evaluates the wrapped parser
   * [[p]] only when `parse` is called to allow for circular
   * dependencies between parsers.
   */
  case class Rule[+T, Elem, Repr](name: String, p: () => Parser[T, Elem, Repr])
                                 (implicit repr: ReprOps[Elem, Repr])
    extends Parser[T, Elem, Repr]{
    private[this] lazy val pCached = p()

    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {

      if (cfg.instrument == null) {
        pCached.parseRec(cfg, index) match{
          case f: Mutable.Failure[Elem, Repr] => failMore(f, index, cfg.logDepth)
          case s: Mutable.Success[T, Elem, Repr] => s
        }
      } else {
        lazy val res = pCached.parseRec(cfg, index) match{
          case f: Mutable.Failure[Elem, Repr] => failMore(f, index, cfg.logDepth)
          case s: Mutable.Success[T, Elem, Repr] => s
        }
        cfg.instrument(this, index, () => res.toResult)
        res
      }
    }
    override def toString = name
    override def shortTraced = true
  }

  /**
   * Wraps another parser, succeeding/failing identically
   * but consuming no input
   */
  case class Lookahead[Elem, Repr](p: Parser[_, Elem, Repr])
                                  (implicit repr: ReprOps[Elem, Repr]) extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val oldNoCut = cfg.isNoCut
      cfg.isNoCut = true
      val res = p.parseRec(cfg, index)
      cfg.isNoCut = oldNoCut

      res match{
        case s: Mutable.Success[_, Elem, Repr] =>
          s.cut = false
          success(cfg.success, (), index, s.traceParsers, false)
        case f: Mutable.Failure[Elem, Repr] =>
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
  case class Not[Elem, Repr](p: Parser[_, Elem, Repr])
                            (implicit repr: ReprOps[Elem, Repr]) extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val oldNoCut = cfg.isNoCut
      cfg.isNoCut = true
      val res0 = p.parseRec(cfg, index)
      cfg.isNoCut = oldNoCut
      val res = res0 match{
        case s: Mutable.Success[_, Elem, Repr] => fail(cfg.failure, s.index)
        case f: Mutable.Failure[Elem, Repr] => success(cfg.success, (), index, Set.empty, false)
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
  case class Optional[+T, R, Elem, Repr](p: Parser[T, Elem, Repr])
                                        (implicit ev: Implicits.Optioner[T, R],
                                         repr: ReprOps[Elem, Repr]) extends Parser[R, Elem, Repr]{

    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val oldFork = cfg.isFork
      cfg.isFork = true
      val res = p.parseRec(cfg, index)
      cfg.isFork = oldFork

      res match{
        case s: Mutable.Success[_, Elem, Repr] =>
          success(cfg.success, ev.some(s.value), s.index, s.traceParsers, s.cut)
        case f: Mutable.Failure[Elem, Repr] if f.cut => failMore(f, index, cfg.logDepth)
        case _ => success(cfg.success, ev.none, index, Set.empty, false)
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
   *
   * Appears to speed up the scalaparse.PerfTests benchmark by around 2.5x
   */
  object Sequence{

    /**
     * The contents of a [[Sequence]] node, minus the left subtree.
     */
    case class Chain[R, Elem, Repr](p: Parser[R, Elem, Repr], cut: Boolean)
                                   (val ev: Implicits.Sequencer[R, R, R])

    case class Flat[R, Elem, Repr](p0: Parser[R, Elem, Repr],
                                   ps: ArrayBuffer[Chain[R, Elem, Repr]])
                                  (implicit repr: ReprOps[Elem, Repr])
      extends Parser[R, Elem, Repr] {

      def parseRec(cfg: ParseCtx[Elem, Repr], index: Int): Mutable[R, Elem, Repr] = {
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
                         traceParsers: Set[Parser[_, Elem, Repr]]): Mutable[R, Elem, Repr] = {
          val currParserCut = if (vIndex < ps.length) ps(vIndex).cut else false
          if (rIndex > index && cfg.checkForDrop(rCut | currParserCut)) cfg.input.dropBuffer(rIndex)

          if (vIndex >= ps.length) success(cfg.success, r1, rIndex, traceParsers, rCut)
          else {
            val c = ps(vIndex)
            c.p.parseRec(cfg, rIndex) match {
              case f: Mutable.Failure[Elem, Repr] => failMore(
                f,
                rIndex,
                cfg.logDepth,
                traceParsers = mergeTrace(cfg.traceIndex, f.traceParsers, traceParsers),
                cut = c.cut | f.cut | rCut
              )
              case Mutable.Success(value0, index0, traceParsers0, cut0)  =>
                rec(
                  c.ev(r1, value0),
                  index0,
                  c.cut | cut0 | rCut,
                  vIndex + 1,
                  traceParsers0 | traceParsers
                )
            }
          }
        }
        p0.parseRec(cfg, index) match{
          case f: Mutable.Failure[Elem, Repr] => failMore(f, index, cfg.logDepth, cut = f.cut)
          case Mutable.Success(value0, index0, traceParsers0, cut0) =>
            rec(value0, index0, cut0, 0, traceParsers0)
        }
      }
      override def opPred = Precedence.OtherOp
      override def toString = {

        val rhs = for(c <- ps) yield {
          " ~" + (if (c.cut) "/" else "") + " " + opWrap(c.p)
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
    def flatten[R, Elem, Repr](s: Sequence[R, R, R, Elem, Repr])
                              (implicit repr: ReprOps[Elem, Repr]): Flat[R, Elem, Repr] = {
      def rec(s: Sequence[R, R, R, Elem, Repr]): Flat[R, Elem, Repr] = {
        val ev2 = s.ev2.asInstanceOf[Implicits.Sequencer[R, R, R]]
        s.p1 match{
          case f: Flat[R, Elem, Repr] =>
            f.copy(ps = f.ps :+ Chain[R, Elem, Repr](s.p2, s.cut)(ev2))
          case p: Sequence[R, R, R, Elem, Repr] =>
            val res = rec(p)
            res.copy(ps = res.ps :+ Chain[R, Elem, Repr](s.p2, s.cut)(ev2))
          case p => Flat(p, ArrayBuffer(Chain[R, Elem, Repr](s.p2, s.cut)(ev2)))
        }
      }
      rec(s)
    }
  }

  /**
   * Parsers two things in a row, returning a tuple of the two
   * results if both things succeed
   */
  case class Sequence[+T1, +T2, R,
                      Elem, Repr](p1: Parser[T1, Elem, Repr],
                                  p2: Parser[T2, Elem, Repr], cut: Boolean)
                                 (implicit ev: Implicits.Sequencer[T1, T2, R],
                                  repr: ReprOps[Elem, Repr]) extends Parser[R, Elem, Repr]{
    def ev2: Implicits.Sequencer[_, _, _] = ev
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {

      p1.parseRec(cfg, index) match{
        case f: Mutable.Failure[Elem, Repr] =>
          failMore(
            f, index, cfg.logDepth,
            traceParsers = mergeTrace(cfg.traceIndex, Set(p1), Set.empty),
            cut = f.cut
          )
        case Mutable.Success(value0, index0, traceParsers0, cut0)  =>
          if (index0 > index && cfg.checkForDrop(cut | cut0)) cfg.input.dropBuffer(index0)

          p2.parseRec(cfg, index0) match{
            case f: Mutable.Failure[Elem, Repr] => failMore(
              f,
              index,
              cfg.logDepth,
              traceParsers = mergeTrace(cfg.traceIndex, traceParsers0, f.traceParsers),
              cut = cut | f.cut | cut0
            )
            case Mutable.Success(value1, index1, traceParsers1, cut1)  =>
              if (index1 > index0 && cfg.checkForDrop(cut | cut0 | cut)) cfg.input.dropBuffer(index0)

              success(
                cfg.success, ev(value0, value1), index1,
                mergeTrace(cfg.traceIndex, traceParsers1, traceParsers0),
                cut1 | cut0 | cut
              )
          }
      }
    }
    override def opPred = Precedence.OtherOp
    override def toString = {
      val op = if(cut) "~/" else "~"
      opWrap(p1) + " " + op + " " + opWrap(p2)
    }
  }


  case class Cut[T, Elem, Repr](p: Parser[T, Elem, Repr])
                               (implicit repr: ReprOps[Elem, Repr])extends Parser[T, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      p.parseRec(cfg, index) match{
        case f: Mutable.Failure[Elem, Repr] => failMore(f, index, cfg.logDepth, cut = false)
        case s: Mutable.Success[T, Elem, Repr] =>
          if (s.index > index && !cfg.isCapturing && !cfg.isNoCut)
            cfg.input.dropBuffer(s.index)
          success(s, s.value, s.index, s.traceParsers, cut = true)
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
  case class Repeat[T, +R, Elem, Repr](p: Parser[T, Elem, Repr], min: Int, max: Int,
                                       delimiter: Parser[_, Elem, Repr])
                                      (implicit ev: Implicits.Repeater[T, R],
                                       repr: ReprOps[Elem, Repr]) extends Parser[R, Elem, Repr]{


    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      @tailrec def rec(index: Int,
                       del: Parser[_, Elem, Repr],
                       lastFailure: Mutable.Failure[Elem, Repr],
                       acc: ev.Acc,
                       cut: Boolean,
                       count: Int): Mutable[R, Elem, Repr] = {
        val oldFork = cfg.isFork
        cfg.isFork = true
        val resDel = del.parseRec(cfg, index)
        cfg.isFork = oldFork

        resDel match{
          case f1: Mutable.Failure[Elem, Repr] =>
            val cut1 = f1.cut
            if (cut1) failMore(f1, index, cfg.logDepth, cut = true)
            else passInRange(cut, f1, index, ev.result(acc), count)

          case Mutable.Success(value0, index0, traceParsers0, cut0)  =>
            cfg.isFork = true
            val res = p.parseRec(cfg, index0)
            cfg.isFork = oldFork

            res match{
              case f2: Mutable.Failure[Elem, Repr] =>
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
                      lastFailure: Mutable.Failure[Elem, Repr],
                      finalIndex: Int,
                      acc: R,
                      count: Int) = {
        if (min <= count) {
          val parsers =
            if (null == lastFailure) Set.empty[Parser[_, Elem, Repr]]
            else lastFailure.traceParsers
          success(cfg.success, acc, finalIndex, parsers, cut)
        } else failMore(lastFailure, index, cfg.logDepth, cut = cut)
      }

      // don't call the parseRec at all, if max is "0", as our parser corresponds to `Pass` in that case.
      if (max == 0 ) {
        success(cfg.success, ev.result(ev.initial), index, Set.empty[Parser[_, Elem, Repr]], false)
      } else {
        rec(index, Pass[Elem, Repr], null, ev.initial, false, 0)
      }
    }
    override def toString = {
      val things = Seq(
        if (min == 0) None else Some(min),
        if (delimiter == Pass[Elem, Repr]) None else Some("sep = " + delimiter),
        if (max == Int.MaxValue) None else Some("max = " + max)
      ).flatten.mkString(", ")
      if (things.isEmpty) opWrap(p) + ".rep"
      else s"${opWrap(p)}.rep($things)"
    }
  }

  object Either{
    def flatten[T, Elem, Repr](p: Vector[Parser[T, Elem, Repr]]): Vector[Parser[T, Elem, Repr]] = p.flatMap{
      case Either(ps@_*) => ps
      case p => Vector(p)
    }
  }
  /**
   * Parses using one parser or the other, if the first one fails. Returns
   * the first one that succeeds and fails if both fail
   */
  case class Either[T, Elem, Repr](ps: Parser[T, Elem, Repr]*)
                                  (implicit repr: ReprOps[Elem, Repr]) extends Parser[T, Elem, Repr]{
    private[this] val ps0 = ps.toArray
    private[this] val n = ps0.length
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      @tailrec def rec(parserIndex: Int, traceParsers: Set[Parser[_, Elem, Repr]]): Mutable[T, Elem, Repr] = {
        if (parserIndex >= n) fail(cfg.failure, index)
        else {
          var res: Mutable[T, Elem, Repr] = null

          if (parserIndex == n - 1) {
            res = ps0(parserIndex).parseRec(cfg, index)
          } else {
            val oldFork = cfg.isFork
            cfg.isFork = true
            res = ps0(parserIndex).parseRec(cfg, index)
            cfg.isFork = oldFork
          }

          res match {
            case s: Mutable.Success[T, Elem, Repr] =>
              s.traceParsers = mergeTrace(cfg.traceIndex, s.traceParsers, traceParsers)
              s
            case f: Mutable.Failure[Elem, Repr] if f.cut => failMore(f, index, cfg.logDepth)
            case f: Mutable.Failure[Elem, Repr] => rec(
              parserIndex + 1,
              mergeTrace(cfg.traceIndex, f.traceParsers, traceParsers)
            )
          }
        }
      }
      rec(0, Set.empty)
    }
    override def opPred = if (ps.length == 1) ps(0).opPred else Precedence.|
    override def toString = {
      ps.map(opWrap).mkString(" | ")
    }
  }

}
