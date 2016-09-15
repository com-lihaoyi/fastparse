package fastparse
import acyclic.file

import language.experimental.macros
import fastparse.parsers.Intrinsics
import fastparse.utils.{ElemSetHelper, ReprOps}

import scala.reflect.ClassTag
/**
  * This is basically a trait which contains the "public" API to fastparse
  * packages.
  *
  * It aliases all the different parsers available in `fastparse.parsers.*`,
  * as well as many of the other useful types such as `Mutable` and `Parsed`
  * and `ParserInput`, and fixes their type-parameters to `[Elem, Repr]`, so
  * that anyone who uses the aliases defined here will not need to worry about
  * filling in these type parameters every time they want to use it.
  *
  * If someone wants to write a parser that works on both bytes and strings,
  * they still have the option of using the "raw" types and doing that, but
  * most people shouldn't need to bother.
  *
  * Also provides the implicits necessary for people who want to construct
  * their own parsers, in the cases where we couldn't provide the implicit
  * directly, e.g. for people defining their own subclass of `Parser`
  */
abstract class Api[Elem, Repr](ct: ClassTag[Elem],
                               elemSetHelper: ElemSetHelper[Elem],
                               reprOps: ReprOps[Elem, Repr],
                               ordering: Ordering[Elem]) {

  implicit val implicitReprOps = reprOps
  implicit val implicitElemSetHelper = elemSetHelper
  protected[this] implicit val implicitClassTag = ct
  protected[this] implicit val implicitOrdering = ordering

  type ParserInput = fastparse.utils.ParserInput[Elem, Repr]
  type IndexedParserInput = fastparse.utils.IndexedParserInput[Elem, Repr]
  type IteratorParserInput = fastparse.utils.IteratorParserInput[Elem, Repr]

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
