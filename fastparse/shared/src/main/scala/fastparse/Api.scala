package fastparse

import fastparse.core.{ParserApi, ParserApiImpl}

import language.experimental.macros
import fastparse.parsers.Intrinsics
import fastparse.parsers.Terminals.AnyElems
import fastparse.utils.{ElemSetHelper, ElemFormatter, ResultConverter}
/**
 * This is basically a trait which contains
 * the "public" API to fastparse packages
 */

abstract class Api[Elem, Repr]()(implicit elemSetHelper: ElemSetHelper[Elem],
                                     elemFormatter: ElemFormatter[Elem, Repr],
                                     converter: ResultConverter[Elem, Repr],
                                     ordering: Ordering[Elem]) {
  type ParseCtx = core.ParseCtx[Elem, Repr]
  object Mutable{
    type Success[T] = core.Mutable.Success[T, Elem, Repr]
    val Success = core.Mutable.Success
    type Failure = core.Mutable.Failure[Elem, Repr]
    val Failure = core.Mutable.Failure
  }

  type Parsed[+T] = core.Parsed[T, Elem, Repr]
  object Parsed {
    type Failure = core.Parsed.Failure[Elem, Repr]
    type Success[T] = core.Parsed.Success[T, Elem, Repr]
    val Success = core.Parsed.Success
    val Failure = core.Parsed.Failure
  }

  val Pass = parsers.Terminals.Pass[Elem, Repr]()
  def PassWith[T](t: T) = parsers.Terminals.PassWith[T, Elem, Repr](t)
  val Fail = parsers.Terminals.Fail[Elem, Repr]()
  val Start = parsers.Terminals.Start[Elem, Repr]()
  val End = parsers.Terminals.End[Elem, Repr]()
  val Index = parsers.Terminals.Index[Elem, Repr]()
  val AnyElem: P0
  def ElemPred(pred: Elem => Boolean): P0
  def ElemIn(seqs: Seq[Elem]*): P0
  def ElemsWhile(pred: Elem => Boolean, min: Int = 1): P0

  def SeqIn(seqs: Seq[Elem]*) = Intrinsics.StringIn[Elem, Repr](seqs.map(_.toIndexedSeq): _*)

  val NoTrace = parsers.Combinators.NoTrace
  val NoCut = parsers.Combinators.NoCut

  def &(p: P[_]) = parsers.Combinators.Lookahead(p)

  def P[T](p: => Parser[T])(implicit name: sourcecode.Name): Parser[T] =
    parsers.Combinators.Rule(name.value, () => p)

  type P0 = Parser[Unit]
  type Parser[+T] = core.Parser[T, Elem, Repr]
  type P[+T] = Parser[T]
  val ParseError = core.ParseError[Elem, Repr] _
  type ParseError = core.ParseError[Elem, Repr]
}

class StringApi() extends Api[Char, String]() {

  val AnyChar = parsers.Terminals.AnyElem[Char, String]("AnyChar")
  def AnyChars(count: Int) = AnyElems[Char, String]("AnyChars", count)

  val AnyElem = AnyChar
  def AnyElem(count: Int) = AnyChars(count)
  def CharPred(pred: Char => Boolean): P0 = Intrinsics.ElemPred("CharPred", pred)
  def CharIn(strings: Seq[Char]*) = Intrinsics.ElemIn[Char, String]("CharIn", strings.map(_.toIndexedSeq): _*)
  def CharsWhile(pred: Char => Boolean, min: Int = 1) = Intrinsics.ElemsWhile[Char, String]("CharsWhile", pred, min)


  def ElemPred(pred: Char => Boolean) = CharPred(pred)
  def ElemIn(strings: Seq[Char]*) = CharIn(strings:_*)
  def ElemsWhile(pred: Char => Boolean, min: Int = 1) = CharsWhile(pred, min)


  def StringIn(strings: Seq[Char]*) = SeqIn(strings: _*)

  val CharPredicates = fastparse.CharPredicates
  val IgnoreCase = parsers.Terminals.IgnoreCase

  implicit def wspStr(s: String): P0 =
    if (s.length == 1) parsers.Terminals.ElemLiteral(s(0))
    else parsers.Terminals.Literal(s)
}

object all extends StringApi{
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Char, String]): ParserApi[V, Char, String] =
    new ParserApiImpl[V, Char, String](p)
}
object noApi extends StringApi
