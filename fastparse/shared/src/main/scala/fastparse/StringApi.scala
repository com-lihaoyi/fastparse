package fastparse
import acyclic.file
import fastparse.core.{ParserApi, ParserApiImpl}

import language.experimental.macros
import fastparse.parsers.Intrinsics
import fastparse.parsers.Terminals.AnyElems
import fastparse.utils.ElemSetHelper.CharBitSetHelper


class StringApi() extends Api[Char, String](
  implicitly, CharBitSetHelper, StringReprOps, CharBitSetHelper.ordering
) {

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


  def StringIn(strings: String*) = SeqIn(strings: _*)
  def StringInIgnoreCase(strings: String*) =
    Intrinsics.StringInIgnoreCase[Char, String](strings: _*)

  val CharPredicates = fastparse.CharPredicates
  val IgnoreCase = parsers.Terminals.IgnoreCase

  implicit def LiteralStr(s: String): P0 =
    if (s.length == 1) parsers.Terminals.ElemLiteral(s(0))
    else parsers.Terminals.Literal(s)
}

object all extends StringApi{
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Char, String]): ParserApi[V, Char, String] =
    new ParserApiImpl[V, Char, String](p)
}
object noApi extends StringApi
