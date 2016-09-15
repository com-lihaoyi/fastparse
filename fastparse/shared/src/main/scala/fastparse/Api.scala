package fastparse

import fastparse.core.{ParserApi, ParserApiImpl}

import language.experimental.macros
import fastparse.parsers.Intrinsics
import fastparse.parsers.Terminals.AnyElems
import fastparse.utils.{ElemSetHelper, ElemTypeFormatter, ResultConverter}
/**
 * This is basically a trait which contains
 * the "public" API to fastparse packages
 */

abstract class Api[ElemType, Repr]()(implicit elemSetHelper: ElemSetHelper[ElemType],
                                     elemFormatter: ElemTypeFormatter[ElemType],
                                     converter: ResultConverter[ElemType, Repr],
                                     ordering: Ordering[ElemType]) {
  type Parsed[+T] = core.Parsed[T, ElemType]
  object Parsed {
    type Failure = core.Parsed.Failure[ElemType]
    type Success[T] = core.Parsed.Success[T, ElemType]
    val Success = core.Parsed.Success
    val Failure = core.Parsed.Failure
  }

  val Pass = parsers.Terminals.Pass[ElemType, Repr]()
  def PassWith[T](t: T) = parsers.Terminals.PassWith[T, ElemType, Repr](t)
  val Fail = parsers.Terminals.Fail[ElemType, Repr]()
  val Start = parsers.Terminals.Start[ElemType, Repr]()
  val End = parsers.Terminals.End[ElemType, Repr]()
  val Index = parsers.Terminals.Index[ElemType, Repr]()
  val AnyElem: P0
  def ElemPred(pred: ElemType => Boolean): P0
  def ElemIn(seqs: Seq[ElemType]*): P0
  def ElemsWhile(pred: ElemType => Boolean, min: Int = 1): P0

  def SeqIn(seqs: Seq[ElemType]*) = Intrinsics.StringIn[ElemType, Repr](seqs.map(_.toIndexedSeq): _*)

  val NoTrace = parsers.Combinators.NoTrace
  val NoCut = parsers.Combinators.NoCut

  def &(p: P[_]) = parsers.Combinators.Lookahead(p)

  def P[T](p: => Parser[T])(implicit name: sourcecode.Name): Parser[T] =
    parsers.Combinators.Rule(name.value, () => p)

  type P0 = Parser[Unit]
  type Parser[+T] = core.Parser[T, ElemType, Repr]
  type P[+T] = Parser[T]
  val ParseError = core.ParseError[ElemType] _
  type ParseError = core.ParseError[ElemType]
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
  val IgnoreCase = parsers.Terminals.IgnoreCase[String] _

  implicit def wspStr(s: String): P0 =
    if (s.length == 1) parsers.Terminals.ElemLiteral(s(0))
    else parsers.Terminals.Literal(s)
}

object all extends StringApi{
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Char, String]): ParserApi[V, Char, String] =
    new ParserApiImpl[V, Char, String](p)
}
object noApi extends StringApi
