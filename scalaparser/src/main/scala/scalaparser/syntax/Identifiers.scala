package scalaparser.syntax

import acyclic.file
import fastparse.Parser.CharsWhile
import fastparse._
import Basic._
object Identifiers{

  val Operator = R( !Keywords ~ CharsWhile(isOpChar, min = 1) )

  val VarId = VarId0(true)

  def VarId0(dollar: Boolean) = R( !Keywords ~ Lower ~ IdRest(dollar) )
  val PlainId = R( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator ~ !OpChar )
  val PlainIdNoDollar = R( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
  val BacktickId = R( "`" ~ CharsWhile(_ != '`', min = 1) ~ "`" )
  val Id: R0 = R( BacktickId | PlainId )

  def IdRest(allowDollar: Boolean) = {
    val NonLetterDigitId = if(!allowDollar) "" else "$"
    val IdUnderscoreChunk = R( CharsWhile(_ ==  '_', min = 0) ~ CharsWhile(
      c => NonLetterDigitId.contains(c) || c.isLetterOrDigit,
      min = 1
    ) )
    R( IdUnderscoreChunk.rep ~ (CharsWhile(_ == '_', min = 1) ~ CharsWhile(isOpChar)).? )
  }

  val alphaKeywords = Seq(
    "abstract", "case", "catch", "class", "def", "do", "else",
    "extends", "false", "finally", "final", "finally", "forSome",
    "for", "if", "implicit", "import", "lazy", "match", "new",
    "null", "object", "override", "package", "private", "protected",
    "return", "sealed", "super", "this", "throw", "trait", "try",
    "true", "type", "val", "var", "while", "with", "yield", "_"
  )

  val AlphabetKeywords = R {
    StringIn(alphaKeywords:_*) ~ !Letter
  }
  val symbolKeywords = Seq(
    ":", ";", "=>", "=", "<-", "<:", "<%", ">:", "#", "@", "\u21d2", "\u2190"
  ) 
  val SymbolicKeywords = R{
    StringIn(symbolKeywords:_*) ~ !OpChar
  }
  val Keywords = R( AlphabetKeywords | SymbolicKeywords )
}
