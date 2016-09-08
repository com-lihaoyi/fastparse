package fastparse.parsers
import acyclic.file
import fastparse.{ElemTypeFormatter, ParserInput}
import fastparse.ElemTypeFormatter._
import fastparse.Utils._
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
  case class Pass[ElemType, Repr]() extends Parser[Unit, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = success(cfg.success, (), index, Set.empty, false)
    override val toString = "Pass"
  }

  /**
    * A parser that always succeeds with given result value `t`, consuming no input
    */
  case class PassWith[T, ElemType, Repr](t: T) extends Parser[T, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = success(cfg.success, t, index, Set.empty, false)
  }

  /**
   * A parser that always fails immediately
   */
  case class Fail[ElemType, Repr]() extends Parser[Nothing, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = fail(cfg.failure, index)
    override val toString = "Fail"
  }
  /**
   * Succeeds, consuming a single element
   */
  case class AnyElem[ElemType, Repr](name: String) extends Parser[Unit, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else success(cfg.success, (), index+1, Set.empty, false)
    }
    override val toString = name
  }
  /**
   * Consumes up to `count` elements, if they are available
   */
  case class AnyElems[ElemType, Repr](name: String, count: Int) extends Parser[Unit, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index + count - 1)) fail(cfg.failure, index)
      else success(cfg.success, (), index + count, Set.empty, false)
    }
    override val toString = name + "(" + count + ")"
  }

  /**
   * Succeeds if at the start of the input, consuming no input
   */

  case class Start[ElemType, Repr]() extends Parser[Unit, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      if (index == 0) success(cfg.success, (), index, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override val toString = "Start"
  }

  /**
   * Succeeds if at the end of the input, consuming no input
   */
  case class End[ElemType, Repr]() extends Parser[Unit, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      if (!cfg.input.isReachable(index)) success(cfg.success, (), index, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override val toString = "End"
  }

  /**
   * Workaround https://github.com/scala-js/scala-js/issues/1603
   * by implementing startsWith myself
   */
  def startsWith[ElemType](src: ParserInput[ElemType], prefix: IndexedSeq[ElemType], offset: Int) = {
    @tailrec def rec(i: Int): Boolean = {
      if (i >= prefix.length) true
      else if (!src.isReachable(i + offset)) false
      else if (src(i + offset) != prefix(i)) false
      else rec(i + 1)
    }
    rec(0)
  }

  def startsWithIgnoreCase(src: ParserInput[Char], prefix: IndexedSeq[Char], offset: Int) = {
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
   * Parses a literal `IndexedSeq[ElemType]`
   */
  case class Literal[ElemType, Repr](s: IndexedSeq[ElemType])
                                    (implicit formatter: ElemTypeFormatter[ElemType])
       extends Parser[Unit, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {

      if (startsWith(cfg.input, s, index)) success(cfg.success, (), index + s.length, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = formatter.literalize(s)
  }

  /**
   * Parses a literal `String` ignoring case
   */
  case class IgnoreCase[Repr](s: IndexedSeq[Char])
                             (implicit formatter: ElemTypeFormatter[Char])
       extends Parser[Unit, Char, Repr]{

    def parseRec(cfg: ParseCtx[Char, Repr], index: Int) = {
      if (startsWithIgnoreCase(cfg.input, s, index)) success(cfg.success, (), index + s.length, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = formatter.literalize(s)
  }

  /**
   * Parses a single element
   */
  case class ElemLiteral[ElemType, Repr](c: ElemType)
                                        (implicit formatter: ElemTypeFormatter[ElemType])
       extends Parser[Unit, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else if (input(index) == c) success(cfg.success, c.toString, index + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = formatter.literalize(ArrayBuffer(c)).toString
  }

  /**
   * Always succeeds, and provides the current index of the
   * parse into the input. e.g. useful for providing
   * source locations for AST nodes. Consumes no input.
   */
  case class Index[ElemType, Repr]() extends Parser[Int, ElemType, Repr]{
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      success(cfg.success, index, index, Set.empty, false)
    }
  }
}
