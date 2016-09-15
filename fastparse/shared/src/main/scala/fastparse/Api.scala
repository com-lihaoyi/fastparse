package fastparse
import acyclic.file
import fastparse.core.{ParserApi, ParserApiImpl}

import language.experimental.macros
import fastparse.parsers.Intrinsics
import fastparse.parsers.Terminals.AnyElems
import fastparse.utils.{ElemSetHelper, ReprOps}

import scala.reflect.ClassTag
/**
 * This is basically a trait which contains
 * the "public" API to fastparse packages
 */

abstract class Api[Elem, Repr](ct: ClassTag[Elem],
                               elemSetHelper: ElemSetHelper[Elem],
                               reprOps: ReprOps[Elem, Repr],
                               ordering: Ordering[Elem]) {

  implicit val implicitReprOps = reprOps
  implicit val implicitElemSetHelper = elemSetHelper
  protected[this] implicit val implicitClassTag = ct
  protected[this] implicit val implicitOrdering = ordering

  class IndexedParserInput(data: Repr)
  extends fastparse.utils.IndexedParserInput[Elem, Repr](data)

  class IteratorParserInput(data: Iterator[Repr])
  extends fastparse.utils.IteratorParserInput[Elem, Repr](data)

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

  def SeqIn(seqs: Repr*) = Intrinsics.StringIn[Elem, Repr](seqs: _*)

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
