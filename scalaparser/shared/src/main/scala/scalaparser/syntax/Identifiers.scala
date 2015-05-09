package scalaparser.syntax

import acyclic.file
import fastparse._
import Basic._
object Identifiers{

  val Operator = P( !Keywords ~ CharsWhile(isOpChar, min = 1) )

  val VarId = VarId0(true)

  def VarId0(dollar: Boolean) = P( !Keywords ~ Lower ~ IdRest(dollar) )
  val PlainId = P( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator ~ !OpChar )
  val PlainIdNoDollar = P( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
  val BacktickId = P( "`" ~ CharsWhile(_ != '`', min = 1) ~ "`" )
  val Id: P0 = P( BacktickId | PlainId )

  def IdRest(allowDollar: Boolean) = {
    val NonLetterDigitId = if(!allowDollar) "" else "$"
    val IdUnderscoreChunk = P( CharsWhile(_ ==  '_', min = 0) ~ CharsWhile(
      c => NonLetterDigitId.contains(c) || CharPredicates.isLetter(c) || CharPredicates.isDigit(c),
      min = 1
    ) )
    P( IdUnderscoreChunk.rep ~ (CharsWhile(_ == '_', min = 1) ~ CharsWhile(isOpChar)).? )
  }

  val alphaKeywords = Seq(
    "abstract", "case", "catch", "class", "def", "do", "else",
    "extends", "false", "finally", "final", "finally", "forSome",
    "for", "if", "implicit", "import", "lazy", "match", "new",
    "null", "object", "override", "package", "private", "protected",
    "return", "sealed", "super", "this", "throw", "trait", "try",
    "true", "type", "val", "var", "while", "with", "yield", "_"
  )

  val AlphabetKeywords = P {
    StringIn(alphaKeywords:_*) ~ !Letter
  }
  val symbolKeywords = Seq(
    ":", ";", "=>", "=", "<-", "<:", "<%", ">:", "#", "@", "\u21d2", "\u2190"
  ) 
  val SymbolicKeywords = P{
    StringIn(symbolKeywords:_*) ~ !OpChar
  }
  val Keywords = P( AlphabetKeywords | SymbolicKeywords )
}
