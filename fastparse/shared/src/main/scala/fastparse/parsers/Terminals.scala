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
  case class Pass[ElemType, R]() extends Parser[Unit, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = success(cfg.success, (), index, Set.empty, false)
    override val toString = "Pass"
  }

  /**
    * A parser that always succeeds with given result value `t`, consuming no input
    */
  case class PassWith[T, ElemType, R](t: T) extends Parser[T, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = success(cfg.success, t, index, Set.empty, false)
  }

  /**
   * A parser that always fails immediately
   */
  case class Fail[ElemType, R]() extends Parser[Nothing, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = fail(cfg.failure, index)
    override val toString = "Fail"
  }
  /**
   * Succeeds, consuming a single element
   */
  case class AnyElem[ElemType, R]() extends Parser[Unit, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else success(cfg.success, input(index), index+1, Set.empty, false)
    }
    override val toString = "AnyElem"
  }

  /**
   * Succeeds if at the start of the input, consuming no input
   */

  case class Start[ElemType, R]() extends Parser[Unit, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      if (index == 0) success(cfg.success, (), index, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override val toString = "Start"
  }

  /**
   * Succeeds if at the end of the input, consuming no input
   */
  case class End[ElemType, R]() extends Parser[Unit, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
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
  case class Literal[ElemType, R](s: IndexedSeq[ElemType])
                              (implicit formatter: ElemTypeFormatter[ElemType])
       extends Parser[Unit, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {

      if (startsWith(cfg.input, s, index)) success(cfg.success, (), index + s.length, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = formatter.literalize(s)
  }

  /**
   * Parses a literal `String` ignoring case
   */
  case class IgnoreCase[R](s: IndexedSeq[Char])
                       (implicit formatter: ElemTypeFormatter[Char])
       extends Parser[Unit, Char, R]{

    def parseRec(cfg: ParseCtx[Char], index: Int) = {
      if (startsWithIgnoreCase(cfg.input, s, index)) success(cfg.success, (), index + s.length, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = formatter.literalize(s)
  }

  /**
   * Parses a single element
   */
  case class ElemLiteral[ElemType, R](c: ElemType)
                                     (implicit formatter: ElemTypeFormatter[ElemType])
       extends Parser[Unit, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
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
  case class Index[ElemType, R]() extends Parser[Int, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      success(cfg.success, index, index, Set.empty, false)
    }
  }
}
