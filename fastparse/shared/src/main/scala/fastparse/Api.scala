package fastparse
import language.experimental.macros
import fastparse.parsers.{Intrinsics, Terminals}
import acyclic.file
import fastparse.Utils.ElemHelper
import fastparse.parsers.Intrinsics.ElemsWhile

/**
 * This is basically a trait which contains
 * the "public" API to fastparse packages
 */

trait Api[ElemType, Repr] {
  type Parsed[+T] = core.Parsed[T, ElemType]
  object Parsed {
    type Failure = core.Parsed.Failure[ElemType]
    type Success[T] = core.Parsed.Success[T, ElemType]
    val Success = core.Parsed.Success
    val Failure = core.Parsed.Failure
  }

  val Pass = parsers.Terminals.Pass[ElemType, Repr]()
  val Fail = parsers.Terminals.Fail[ElemType, Repr]()
  val Start = parsers.Terminals.Start[ElemType, Repr]()
  val End = parsers.Terminals.End[ElemType, Repr]()
  val Index = parsers.Terminals.Index[ElemType, Repr]()
  val AnyElem = parsers.Terminals.AnyElem[ElemType, Repr]()

  implicit val elemHelper: ElemHelper[ElemType]
  implicit val parserHelper: ParserHelper[ElemType]
  implicit val ordering: Ordering[ElemType]

  val ElemPred = Intrinsics.ElemPred[ElemType, Repr] _
  def ElemIn(seqs: Seq[ElemType]*) = Intrinsics.ElemIn[ElemType, Repr](seqs.map(_.toIndexedSeq): _*)
  def ElemsWhile(pred: ElemType => Boolean, min: Int = 1) = Intrinsics.ElemsWhile[ElemType, Repr](pred, min)
  def SeqIn(seqs: Seq[ElemType]*) = Intrinsics.StringIn[ElemType, Repr](seqs.map(_.toIndexedSeq): _*)

  val NoTrace = parsers.Combinators.NoTrace
  val NoCut = parsers.Combinators.NoCut

  val & = parsers.Combinators.Lookahead

  def P[T](p: => Parser[T])(implicit name: sourcecode.Name): Parser[T] =
    parsers.Combinators.Rule(name.value, () => p)

  type P0 = Parser[Unit]
  type Parser[+T] = core.Parser[T, ElemType, Repr]
  type P[+T] = Parser[T]
  val ParseError = core.ParseError[ElemType] _
  type ParseError = core.ParseError[ElemType]
}

trait StringApi extends Api[Char, String] {

  val elemHelper = implicitly[ElemHelper[Char]]
  val parserHelper = implicitly[ParserHelper[Char]]
  val ordering = implicitly[Ordering[Char]]

  val AnyChar = AnyElem

  val CharPred = ElemPred
  def CharIn(strings: Seq[Char]*) = ElemIn(strings: _*)
  def CharsWhile(pred: Char => Boolean, min: Int = 1) = ElemsWhile(pred, min)
  def StringIn(strings: Seq[Char]*) = SeqIn(strings: _*)

  val CharPredicates = fastparse.CharPredicates
  val IgnoreCase = parsers.Terminals.IgnoreCase[String] _

  implicit def wspStr(s: String): P0 =
    if (s.length == 1) parsers.Terminals.ElemLiteral(s(0))
    else parsers.Terminals.Literal(s)
}

object allString extends StringApi{
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Char, String]): ParserApi[V, Char, String] =
    new ParserApiImpl[V, Char, String](p)
}
object emptyStringApi extends StringApi


trait ByteApi extends Api[Byte, Array[Byte]] {

  val elemHelper = implicitly[ElemHelper[Byte]]
  val parserHelper = implicitly[ParserHelper[Byte]]
  val ordering = implicitly[Ordering[Byte]]

  val BytePred = ElemPred
  def ByteIn(seqs: Seq[Byte]*) = ElemIn(seqs: _*)
  def BytesWhile(pred: Byte => Boolean, min: Int = 1) = ElemsWhile(pred, min)

  def ByteSeq(bytes: Byte*) = bytes.toArray
  def BS(bytes: Byte*) = ByteSeq(bytes: _*)

  implicit def wspByteSeq(seq: Array[Byte]): P0 =
    if (seq.length == 1) parsers.Terminals.ElemLiteral(seq(0))
    else parsers.Terminals.Literal(seq)
}

object allByte extends ByteApi{
  implicit def parserApi[T, V](p: T)
                              (implicit c: T => core.Parser[V, Byte, Array[Byte]]): ParserApi[V, Byte, Array[Byte]] =
    new ParserApiImpl[V, Byte, Array[Byte]](p)
}
object emptyByteApi extends ByteApi