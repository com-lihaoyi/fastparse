package fastparse.parsers
import acyclic.file
import fastparse.utils.{ParserInput, ReprOps}
import fastparse.core.ParseCtx

import scala.annotation.tailrec
import fastparse.core.Parser

import scala.collection.mutable.ArrayBuffer
/**
 * Leaf parsers which do not contain any other
 * parsers, and do simple things
 */
object Terminals {

  /**
   * A parser that always succeeds, consuming no input
   */
  case class Pass[Elem, Repr]()(implicit repr: ReprOps[Elem, Repr]) extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = success(cfg.success, (), index, Set.empty, false)
    override val toString = "Pass"
  }

  /**
    * A parser that always succeeds with given result value `t`, consuming no input
    */
  case class PassWith[T, Elem, Repr](t: T)(implicit repr: ReprOps[Elem, Repr]) extends Parser[T, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = success(cfg.success, t, index, Set.empty, false)
  }

  /**
   * A parser that always fails immediately
   */
  case class Fail[Elem, Repr]()(implicit repr: ReprOps[Elem, Repr]) extends Parser[Nothing, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = fail(cfg.failure, index)
    override val toString = "Fail"
  }
  /**
   * Succeeds, consuming a single element
   */
  case class AnyElem[Elem, Repr](name: String)(implicit repr: ReprOps[Elem, Repr]) extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else success(cfg.success, (), index+1, Set.empty, false)
    }
    override val toString = name
  }
  /**
   * Consumes up to `count` elements, if they are available
   */
  case class AnyElems[Elem, Repr](name: String, count: Int)(implicit repr: ReprOps[Elem, Repr]) extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index + count - 1)) fail(cfg.failure, index)
      else success(cfg.success, (), index + count, Set.empty, false)
    }
    override val toString = name + "(" + count + ")"
  }

  /**
   * Succeeds if at the start of the input, consuming no input
   */

  case class Start[Elem, Repr]()(implicit repr: ReprOps[Elem, Repr]) extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      if (index == 0) success(cfg.success, (), index, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override val toString = "Start"
  }

  /**
   * Succeeds if at the end of the input, consuming no input
   */
  case class End[Elem, Repr]()(implicit repr: ReprOps[Elem, Repr]) extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      if (!cfg.input.isReachable(index)) success(cfg.success, (), index, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override val toString = "End"
  }

  /**
   * Workaround https://github.com/scala-js/scala-js/issues/1603
   * by implementing startsWith myself
   */
  def startsWith[Elem, Repr](src: ParserInput[Elem, Repr], prefix: Repr, offset: Int)
                                (implicit repr: ReprOps[Elem, Repr])= {
    @tailrec def rec(i: Int): Boolean = {
      if (i >= repr.length(prefix)) true
      else if (!src.isReachable(i + offset)) false
      else if (src(i + offset) != repr.apply(prefix, i)) false
      else rec(i + 1)
    }
    rec(0)
  }

  def startsWithIgnoreCase(src: ParserInput[Char, String], prefix: IndexedSeq[Char], offset: Int) = {
    val max = prefix.length
    @tailrec def rec(i: Int): Boolean = {
      if (i >= prefix.length) true
      else if (!src.isReachable(i + offset)) false
      else {
        val c1: Char = src(i + offset)
        val c2: Char = prefix(i)
        if (c1 != c2 && c1.toLower != c2.toLower) false
        else rec(i + 1)
      }
    }
    rec(0)
  }

  /**
   * Parses a literal `IndexedSeq[Elem]`
   */
  case class Literal[Elem, Repr](s: Repr)
                                (implicit repr: ReprOps[Elem, Repr])
       extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {

      if (startsWith(cfg.input, s, index)) success(cfg.success, (), index + repr.length(s), Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = repr.literalize(s)
  }

  /**
   * Parses a literal `String` ignoring case
   */
  case class IgnoreCase(s: IndexedSeq[Char])
                       (implicit repr: ReprOps[Char, String])
       extends Parser[Unit, Char, String](){

    def parseRec(cfg: ParseCtx[Char, String], index: Int) = {
      if (startsWithIgnoreCase(cfg.input, s, index)) success(cfg.success, (), index + s.length, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = repr.literalize(s.mkString)
  }

  /**
   * Parses a single element
   */
  case class ElemLiteral[Elem, Repr](c: Elem)
                                    (implicit repr: ReprOps[Elem, Repr])
       extends Parser[Unit, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else if (input(index) == c) success(cfg.success, c.toString, index + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = repr.literalize(repr.fromSingle(c)).toString
  }

  /**
   * Always succeeds, and provides the current index of the
   * parse into the input. e.g. useful for providing
   * source locations for AST nodes. Consumes no input.
   */
  case class Index[Elem, Repr]()(implicit repr: ReprOps[Elem, Repr]) extends Parser[Int, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      success(cfg.success, index, index, Set.empty, false)
    }
  }
}
