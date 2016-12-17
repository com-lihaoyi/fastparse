package fastparse.sequence

import fastparse.core.{ ParserApi, ParserApiImpl }
import fastparse.parsers.Terminals.AnyElems
import scala.reflect.ClassTag
import fastparse.{ core, parsers }

class VectorApi[A]()(implicit evidence: ClassTag[A]) extends SeqApi[A, Vector[A]](implicitly, VectorReprOps[A]) {
  val AnyElem = parsers.Terminals.AnyElem[A, Vector[A]]("AnyElem")
  def AnyElem(count: Int) = AnyElems[A, Vector[A]]("AnyElems", count)
  def ElemPred(pred: A => Boolean): P0 = SeqItemParsers.Intrinsics.ElemPred[A, Vector[A]]("ElemPred", pred)
  def ElemIn(items: Seq[A]*): P0 = SeqItemParsers.Intrinsics.ElemIn[A, Vector[A]]("ElemIn", items: _*)
  def ElemsWhile(pred: A => Boolean, min: Int = 1) = SeqItemParsers.Intrinsics.ElemsWhile[A, Vector[A]]("ElemsWhile", pred, min)
}

/**
 * This class is used to parse a Vector of items
 *
 * People can use it if they needs to Parse a Vector resulting of a preceding
 * text parsed for example or to parse any Vector of data.
 *
 * For example if we need to parse an address,
 * we can have a string parser that produces this sequence :
 *
 * val parsed = Seq(
 *   Text("Scala programming"), NewLine,
 *   Number("123"), Text("rue des Champs Elysees"), NewLine,
 *   Number("75000"), Text("PARIS"), NewLine,
 *   Text("FRANCE"))
 *
 * sealed abstract class Part
 * case class Text(s:String) extends Part
 * case class Number(n: String) extends Part
 * case object NewLine extends Part
 *
 * We could parse this sequence with :
 *
 * object all extends fastparse.sequence.allVector[Part]
 *
 * object PostalAddressParser {
 *   val countries = Seq("CHINA", "FRANCE", "GERMANY", "JAPAN",
 *     "UNITED-KINGDOM")
 *   val nl = ElemIn(NewLine)
 *   val number = F({case Number(v) => v})
 *   val text = F({case Text(v) => v})
 *   val country = F({case Text(v) if countries.contains(v.toUpperCase) =>
 *     v.toUpperCase})
 *
 *   val name = (!nl ~ AnyElem).rep(1) ~ nl
 *   val street = (
 *     (number.filter(_.length<5) ~ text) |
 *     (text ~ number.filter(_.length<5)).swap) ~ nl
 *   val postalCodeAndCity = (
 *     (number.filter(_.length>4) ~ text) |
 *     (text ~ number.filter(_.length>4)).swap) ~ nl
 *
 *   val occidentalPostalAddress = (
 *     name ~ street ~ postalCodeAndCity ~ country).
 *     map(l => PostalAddress(l._4,l._3._1,l._3._2,l._2._1,l._2._2,l._1))
 *   val orientalPostalAddress = (
 *     country ~ postalCodeAndCity ~ street ~ name).
 *     map(l => PostalAddress(l._1,l._2._1,l._2._2,l._3._1,l._3._2,,l._4))
 *
 *   val fullPostalAddress = orientalPostalAddress | occidentalPostalAddress
 * }
 * 
 * case class PostalAddress(country: String, postalCode: String, city: String,
 * streetNumber: String, streetName: String, name: String)
 * 
 * PostalAddressParser.fullPostalAddress.parse(parsed, 0) match {
 *   case Parsed.Success(postalAddress, _) => // Yeah ! Play with postalAddress
 *   case f: Parsed.Failure => // Something goes wrong
 * }
 */
class allVector[A]()(implicit evidence: ClassTag[A]) extends VectorApi[A]() {
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, A, Vector[A]]): ParserApi[V, A, Vector[A]] =
    new ParserApiImpl[V, A, Vector[A]](p)
}
