package fastparse.sequence

import scala.reflect.ClassTag
import fastparse.core
import fastparse.core.{ ParserApi, ParserApiImpl }
import fastparse.parsers
import fastparse.utils.ReprOps
import scala.collection.generic.CanBuildFrom

class SeqApi[Elem](implicit ct: ClassTag[Elem]) extends AbstractSeqApi[Elem, Seq[Elem]](SeqReprOps())

abstract class AbstractSeqApi[Elem, Repr](reprOps: ReprOps[Elem, Repr])(implicit ct: ClassTag[Elem]) {
  implicit val implicitReprOps = reprOps

  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Elem, Repr]): ParserApi[V, Elem, Repr] =
    new ParserApiImpl[V, Elem, Repr](p)

  type ParserInput = fastparse.utils.ParserInput[Elem, Repr]
  type IndexParserInput = fastparse.utils.IndexedParserInput[Elem, Repr]
  type IteratorParserInput = fastparse.utils.IteratorParserInput[Elem, Repr]

  type ParseCtx = core.ParseCtx[Elem, Repr]
  object Mutable {
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
  val AnyElem = SeqItemParsers.Terminals.AnyElemRule[Elem, Repr]()

  def ElemPred(pred: Elem => Boolean): P0 =
    SeqItemParsers.Intrinsics.ElemPred[Elem, Repr]("ElemPred", pred)
  def ElemIn(items: Elem*): P0 =
    SeqItemParsers.Intrinsics.ElemIn[Elem, Repr]("ElemIn", items)
  def ElemsWhile(pred: Elem => Boolean, min: Int = 1) =
    SeqItemParsers.Intrinsics.ElemsWhile[Elem, Repr]("ElemsWhile", pred, min)

  val NoTrace = parsers.Combinators.NoTrace
  val NoCut = parsers.Combinators.NoCut

  def &(p: P[_]) = parsers.Combinators.Lookahead(p)

  def P[T](p: => Parser[T])(implicit name: sourcecode.Name): Parser[T] =
    parsers.Combinators.Rule(name.value, () => p)

  def F[T](pf: PartialFunction[Elem, T])(implicit name: sourcecode.Name): Parser[T] =
    SeqItemParsers.Terminals.PartialFunctionRule(name.value, pf)

  type P0 = Parser[Unit]
  type Parser[+T] = core.Parser[T, Elem, Repr]
  type P[+T] = Parser[T]
  val ParseError = core.ParseError[Elem, Repr] _
  type ParseError = core.ParseError[Elem, Repr]
}