package scalaParser
package syntax
import acyclic.file
import parsing.Parsing._
import Basic._
object Identifiers{

  val Operator = rule{!Keywords ~ OpChar.rep1}

  val VarId = VarId0(true)
  val VarIdFalse = VarId0(false)
  def VarId0(dollar: Boolean) = rule( !Keywords ~ Lower.log("Lower") ~ IdRest(dollar).log("IdRest(dollar)") )
  val PlainId = rule( !Keywords ~ Upper ~ IdRestTrue.log("IdRestTrue") | VarId.log("VarId") | Operator.log("Operator") ).log("PlainId")
  val PlainIdNoDollar = rule( !Keywords ~ Upper ~ IdRestFalse | VarIdFalse | Operator )
  val Id = rule( !Keywords ~ PlainId | ("`" ~ (!'`' ~ Parser.AnyChar).rep1 ~ "`") )

  val IdRestFalse = IdRest(false)
  val IdRestTrue = IdRest(true)
  def IdRest(allowDollar: Boolean) = {
    val SkipChar = if(allowDollar) rule("_") else rule("_" | "$")
    val IdUnderscoreChunk = rule( "_".log("_").rep ~ (!SkipChar.log("SC") ~ Letter.log("Letter") | Digit.log("Digit")).rep1 )
    rule( IdUnderscoreChunk.log("IUC").rep ~ ("_".rep1 ~ OpChar.log("OpChar").rep).? )
  }

  val AlphabetKeywords = rule {
    (
      "abstract" | "case" | "catch" | "class" | "def" | "do" | "else" |
      "extends" | "false" | "finally" | "final" | "finally" | "forSome" |
      "for" | "if" | "implicit" | "import" | "lazy" | "match" | "new" |
      "null" | "object" | "override" | "package" | "private" | "protected" |
      "return" | "sealed" | "super" | "this" | "throw" | "trait" | "try" |
      "true" | "type" | "val" | "var" | "while" | "with" | "yield" | "_"
    ) ~ !Letter
  }
  val SymbolicKeywords = rule{
    (
      ":" | ";" | "=>" | "=" | "<-" | "<:" | "<%" | ">:" | "#" | "@" | "\u21d2" | "\u2190"
    )  ~ !OpChar
  }
  val Keywords = rule( AlphabetKeywords | SymbolicKeywords )
}
