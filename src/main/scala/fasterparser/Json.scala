package fasterparser

import scala.collection.mutable


object Js {
  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends AnyVal with Val
  case class Obj(value: (java.lang.String, Val)*) extends AnyVal with Val
  case class Arr(value: Val*) extends AnyVal with Val
  case class Num(value: Double) extends AnyVal with Val
  case object False extends Val{
    def value = false
  }
  case object True extends Val{
    def value = true
  }
  case object Null extends Val{
    def value = null
  }
}

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
  def apply(t: T) = f(t)
  override def toString() = name

}
object Json{
  import Parse._
  def StringChars[_:Ctx] = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  def space[_:Ctx]         = P( CharsWhileIn(" \r\n").? )
  def digits[_:Ctx]        = P( CharsWhileIn("0123456789"))
  def exponent[_:Ctx]      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  def fractional[_:Ctx]    = P( "." ~ digits )
  def integral[_:Ctx]      = P( "0" | CharIn('1' to '9') ~ digits.? )

  def number[_:Ctx] = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => Js.Num(x.toDouble)
  )

  def `null`[_:Ctx]        = P( "null" ).map(_ => Js.Null)
  def `false`[_:Ctx]       = P( "false" ).map(_ => Js.False)
  def `true`[_:Ctx]        = P( "true" ).map(_ => Js.True)

  def hexDigit[_:Ctx]      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  def unicodeEscape[_:Ctx] = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  def escape[_:Ctx]        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

  def strChars[_:Ctx] = P( CharsWhile(StringChars) )
  def string[_:Ctx] =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

  def array[_:Ctx] =
    P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map(Js.Arr(_:_*))

  def pair[_:Ctx] = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

  def obj[_:Ctx] =
    P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(Js.Obj(_:_*))

  def jsonExpr[_:Ctx]: P[Js.Val] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}