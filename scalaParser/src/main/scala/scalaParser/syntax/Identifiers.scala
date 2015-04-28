package scalaParser
package syntax
import acyclic.file
import parsing._
import Basic._
object Identifiers{

  val Operator = rule{!Keywords ~ OpChar.rep1}

  val VarId = VarId0(true)

  def VarId0(dollar: Boolean) = rule( !Keywords ~ Lower ~ IdRest(dollar) )
  val PlainId = rule( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator )
  val PlainIdNoDollar = rule( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
  val Id = rule( !Keywords ~ PlainId | ("`" ~ (!"`" ~ Parser.AnyChar).rep1 ~ "`") )

  def IdRest(allowDollar: Boolean) = {
    val SkipChar: Parser[_] = if(allowDollar) "_" else CharSets("_$")
    val IdUnderscoreChunk = rule( "_".rep ~ (!SkipChar ~ Letter | Digit ).rep1 )
    rule( IdUnderscoreChunk.rep ~ ("_".rep1 ~ OpChar.rep).? )
  }

  val alphaKeywords = Seq(
    "abstract", "case", "catch", "class", "def", "do", "else",
    "extends", "false", "finally", "final", "finally", "forSome",
    "for", "if", "implicit", "import", "lazy", "match", "new",
    "null", "object", "override", "package", "private", "protected",
    "return", "sealed", "super", "this", "throw", "trait", "try",
    "true", "type", "val", "var", "while", "with", "yield", "_"
  )

  val AlphabetKeywords = rule {
    Dispatcher(alphaKeywords:_*) ~ !Letter
  }
  val symbolKeywords = Seq(
    ":", ";", "=>", "=", "<-", "<:", "<%", ">:", "#", "@", "\u21d2", "\u2190"
  ) 
  val SymbolicKeywords = rule{
    Dispatcher(symbolKeywords:_*)  ~ !OpChar
  }
  val Keywords = rule( AlphabetKeywords | SymbolicKeywords )
}
