package fastparse.sequence

import scala.reflect.ClassTag
import fastparse.core
import fastparse.parsers

/**
 * This is a trait applicable on potentially any sequences but Vector or Array
 * are typically usable in real world.
 */
abstract class SeqApi[Elem, Repr <: Seq[Elem]](ct: ClassTag[Elem], reprOps: fastparse.utils.ReprOps[Elem, Repr]) {
  implicit val implicitReprOps = reprOps

  protected[this] implicit val implicitClassTag = ct
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
  val AnyElem: P0
  def ElemPred(pred: Elem => Boolean): P0
  def ElemIn(seqs: Seq[Elem]*): P0
  def ElemsWhile(pred: Elem => Boolean, min: Int = 1): P0

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