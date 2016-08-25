package fastparse
import language.experimental.macros
import fastparse.parsers.{Intrinsics, Terminals}
import fastparse.Utils.HexUtils

/**
 * This is basically a trait which contains
 * the "public" API to fastparse packages
 */

class Api[ElemType, Repr]()(implicit elemSetHelper: ElemSetHelper[ElemType],
                            elemFormatter: ElemTypeFormatter[ElemType],
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
  val AnyElem = parsers.Terminals.AnyElem[ElemType, Repr]()


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

class StringApi() extends Api[Char, String]() {

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

object all extends StringApi{
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Char, String]): ParserApi[V, Char, String] =
    new ParserApiImpl[V, Char, String](p)
}
object noApi extends StringApi


class ByteApi() extends Api[Byte, Array[Byte]]() {

  val AnyByte = AnyElem

  val BytePred = ElemPred
  def ByteIn(seqs: Seq[Byte]*) = ElemIn(seqs: _*)
  def BytesWhile(pred: Byte => Boolean, min: Int = 1) = ElemsWhile(pred, min)

  def ByteSeq[T](bytes: T*)(implicit num: Numeric[T]) = bytes.map(b => num.toInt(b).toByte).toArray
  def BS[T](bytes: T*)(implicit num: Numeric[T]) = ByteSeq[T](bytes: _*)
  type ByteSeq = Array[Byte]
  type BS = ByteSeq

  implicit def wspByteSeq(seq: Array[Byte]): P0 =
    if (seq.length == 1) parsers.Terminals.ElemLiteral(seq(0))
    else parsers.Terminals.Literal(seq)

  object BytesParser {
    import all._

    val hexChars = HexUtils.hexChars

    def charsToByte(s: String): Byte = {
      (hexChars.indexOf(s(1)) + hexChars.indexOf(s(0)) * 16).toByte
    }

    val hexDigit = all.P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
    val byte = all.P("0x".? ~ hexDigit.rep(exactly = 2).!).map(s => charsToByte(s.toLowerCase))
    val byteSep = all.P(" ".rep)
    val bytes = all.P(byteSep ~ byte.rep(sep = byteSep)).map(_.toArray)
  }

  def strToBytes(s: String): Array[Byte] = {
    BytesParser.bytes.parse(s).get.value
  }
}

object allByte extends ByteApi{
  implicit def parserApi[T, V](p: T)
                              (implicit c: T => core.Parser[V, Byte, Array[Byte]]): ParserApi[V, Byte, Array[Byte]] =
    new ParserApiImpl[V, Byte, Array[Byte]](p)

  val AnyWord = P( AnyByte.rep(exactly=2) )
  val AnyDword = P( AnyByte.rep(exactly=4) )
}
object emptyByteApi extends ByteApi