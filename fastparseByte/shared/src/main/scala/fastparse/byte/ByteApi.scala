package fastparse.byte
import acyclic.file
import fastparse._
import fastparse.core.ParserApi
import fastparse.parsers.{Intrinsics, Terminals}
import scodec.bits.ByteVector


class ByteApi() extends Api[Byte, ByteVector](
  implicitly, ByteBitSetHelper, ByteReprOps, ByteBitSetHelper.ordering
) {

  val AnyByte = parsers.Terminals.AnyElem[Byte, Bytes]("AnyByte")
  def AnyBytes(count: Int) = Terminals.AnyElems[Byte, Bytes]("AnyBytes", count)
  def BytePred(pred: Byte => Boolean): P0 = Intrinsics.ElemPred("BytePred", pred)
  def ByteIn(seqs: Seq[Byte]*) = Intrinsics.ElemIn[Byte, Bytes]("ByteIn", seqs.map(_.toIndexedSeq): _*)
  def BytesWhile(pred: Byte => Boolean, min: Int = 1) = Intrinsics.ElemsWhile[Byte, Bytes]("BytesWhile", pred, min)


  val AnyElem = AnyByte
  def AnyElems(count: Int) = AnyBytes(count)
  def ElemPred(pred: Byte => Boolean) = BytePred(pred)
  def ElemIn(strings: Seq[Byte]*) = ByteIn(strings:_*)
  def ElemsWhile(pred: Byte => Boolean, min: Int = 1) = BytesWhile(pred, min)

  /**
    * Construct a literal byte-parser out of raw byte values. Any integral
    * values can be used, but they will be truncated down to `Byte`s before
    * being used in the parser
    */
  def BS[T: Integral](bytes: T*): P0 = {
    parsers.Terminals.Literal[Byte, Bytes](ByteVector(bytes:_*))
  }

  /**
    * Construct a literal byte-parser out of an immutable `Bytes` value
    */
  def BS[T](bytes: Bytes): P0 = {
    parsers.Terminals.Literal[Byte, Bytes](bytes)
  }

  /**
    * Convenient, more-concise alias for `scodec.bits.ByteVector`
    */
  val Bytes = scodec.bits.ByteVector
  type Bytes = scodec.bits.ByteVector

  implicit def HexStringSyntax(sc: StringContext) = new scodec.bits.HexStringSyntax(sc)

  /**
    * Little-endian integer parsers
    */
  val LE = ByteUtils.EndianByteParsers.LE
  /**
    * Big-endian integer parsers
    */
  val BE = ByteUtils.EndianByteParsers.BE
  /**
    * Parses a two-byte word
    */
  val Word16: P[Unit] = new ByteUtils.GenericIntegerParser[Unit](2, (input, n) => ())
  /**
    * Parses a four-byte word
    */
  val Word32: P[Unit] = new ByteUtils.GenericIntegerParser[Unit](4, (input, n) => ())
  /**
    * Parses an eight-byte word
    */
  val Word64: P[Unit] = new ByteUtils.GenericIntegerParser[Unit](8, (input, n) => ())

  val Int8 = ByteUtils.Int8

  val UInt8 = ByteUtils.UInt8
  /**
    * Prettify an array of `bytes` as an easy-to-ready 16-wide grid of hex-values
    * into a string you can print and read.
    *
    * By default, only prints the first 8 rows. You can pass in a set of `markers`
    * in order to label other parts of the input `bytes` with a caret and also print
    * the rows around those points, or set `contextRows` to some other value than
    * 8 if you want to see more or less rows (e.g. set it to Int.MaxValue to show
    * the whole input)
    */
  def prettyBytes(bytes: Bytes,
                  markers: Seq[Int] = Seq(-1),
                  contextRows: Int = 8) = {
    ByteUtils.prettyBytes(bytes, markers, contextRows)
  }
}

object all extends ByteApi {
  implicit def parserApi[T, V](p: T)
                              (implicit c: T => core.Parser[V, Byte, Bytes]): ParserApi[V, Byte, Bytes] =
    new fastparse.core.ParserApiImpl[V, Byte, Bytes](p)

  /**
    * Parses the `sizeParser` to get a number `n`, and then parses `p` exactly
    * that many times, and returns the results as a `Seq`
    */
  def repeatWithSize[Num: Numeric, T](sizeParser: Parser[Num], p: Parser[T]): Parser[Seq[T]] =
  P( sizeParser.flatMap(l => p.rep(exactly = implicitly[Numeric[Num]].toInt(l))) )
}
object noApi extends ByteApi
