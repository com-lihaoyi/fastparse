package scalaParser
package syntax
import acyclic.file
import parsing._
import Basic._
object Identifiers{

  val Operator = R{!Keywords ~ OpChar.rep1}

  val VarId = VarId0(true)

  def VarId0(dollar: Boolean) = R( !Keywords ~ Lower ~ IdRest(dollar) )
  val PlainId = R( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator )
  val PlainIdNoDollar = R( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
  val Id: R0 = R( !Keywords ~ PlainId | ("`" ~ (!"`" ~ Parser.AnyChar).rep1 ~ "`") )

  def IdRest(allowDollar: Boolean) = {
    val SkipChar: Parser[_] = if(allowDollar) "_" else CharIn("_$")
    val IdUnderscoreChunk = R( "_".rep ~ (!SkipChar ~ Letter | Digit ).rep1 )
    R( IdUnderscoreChunk.rep ~ ("_".rep1 ~ OpChar.rep).? )
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
    StringIn(symbolKeywords:_*)  ~ !OpChar
  }
  val Keywords = R( AlphabetKeywords | SymbolicKeywords )
}
