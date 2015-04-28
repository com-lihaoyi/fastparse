package scalaParser
package syntax
import acyclic.file
import parsing._
import Basic._
import Identifiers._

trait Literals {
  def Block: Parser0
  def WL: Parser0
  object Literals{
    import Basic._
    val Float = {
      def Thing = rule( Digit.rep1 ~ Exp.? ~ FloatType.? )
      def Thing2 = rule( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      rule( "." ~ Thing | Digit.rep1 ~ Thing2 )
    }

    val Int = rule( (HexNum | DecNum) ~ CharSets("Ll").? )

    val Bool = rule( Key.W("true") | Key.W("false")  )

    val MultilineComment: Parser0 = rule( "/*" ~ (MultilineComment | !"*/" ~ Parser.AnyChar).rep ~ "*/" )
    val Comment: Parser0 = rule(
      MultilineComment | "//" ~ (!Basic.Newline ~ Parser.AnyChar).rep ~ &(Basic.Newline | Parser.End)
    )
    val Null = Key.W("null")
    val Literal = rule( ("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null )

    val EscapedChars = rule( "\\" ~ CharSets("""btnfr'\"]"""))

    // Note that symbols can take on the same values as keywords!
    val Symbol = rule( "'" ~ (Identifiers.PlainId | Identifiers.Keywords) )

    val Char = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(isPrintableChar)

      rule {
        "'" ~ (UnicodeEscape | EscapedChars | !"\\" ~ PrintableChar) ~ "'"
      }
    }

    val Interp = rule{
      "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ Block ~ WL ~ "}") | "$$"
    }
    val String = {
      import Identifiers.Id
      def InterpIf(allowInterp: Boolean) = rule( if(allowInterp) Interp else Parser.Fail )
      def TQ = rule( "\"\"\"" )
      def TripleChars(allowInterp: Boolean) = rule( (InterpIf(allowInterp) | "\"".? ~ "\"".? ~ !"\"" ~ Parser.AnyChar).rep )
      def TripleTail = rule( TQ ~ "\"".rep )
      def SingleChars(allowInterp: Boolean) = rule( (InterpIf(allowInterp) | "\\\"" | "\\\\" | !CharSets("\n\"") ~ Parser.AnyChar).rep )
      rule {
        (Id ~ TQ ~ TripleChars(allowInterp = true) ~ TripleTail) |
        (Id ~ "\"" ~ SingleChars(allowInterp = true) ~ "\"") |
        (TQ ~ TripleChars(allowInterp = false) ~ TripleTail) |
        ("\"" ~ SingleChars(allowInterp = false) ~ "\"")
      }
    }

    def isPrintableChar(c: Char): Boolean = {
      val block = Character.UnicodeBlock.of(c)
      !Character.isISOControl(c) && !Character.isSurrogate(c) && block != null && block != Character.UnicodeBlock.SPECIALS
    }
  }
}
