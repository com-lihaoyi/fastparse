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
      def Thing = R( Digit.rep1 ~ Exp.? ~ FloatType.? )
      def Thing2 = R( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      R( "." ~ Thing | Digit.rep1 ~ Thing2 )
    }

    val Int = R( (HexNum | DecNum) ~ CharSets("Ll").? )

    val Bool = R( Key.W("true") | Key.W("false")  )

    val MultilineComment: Parser0 = R( "/*" ~ (MultilineComment | !"*/" ~ Parser.AnyChar).rep ~ "*/" )
    val Comment: Parser0 = R(
      MultilineComment | "//" ~ (!Basic.Newline ~ Parser.AnyChar).rep ~ &(Basic.Newline | Parser.End)
    )
    val Null = Key.W("null")
    val Literal = R( ("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null )

    val EscapedChars = R( "\\" ~ CharSets("""btnfr'\"]"""))

    // Note that symbols can take on the same values as keywords!
    val Symbol = R( "'" ~ (Identifiers.PlainId | Identifiers.Keywords) )

    val Char = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(isPrintableChar)

      R {
        "'" ~ (UnicodeEscape | EscapedChars | !"\\" ~ PrintableChar) ~ "'"
      }
    }

    val Interp = R{
      "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ Block ~ WL ~ "}") | "$$"
    }
    val String = {
      import Identifiers.Id
      def InterpIf(allowInterp: Boolean) = R( if(allowInterp) Interp else Parser.Fail )
      def TQ = R( "\"\"\"" )
      def TripleChars(allowInterp: Boolean) = R( (InterpIf(allowInterp) | "\"".? ~ "\"".? ~ !"\"" ~ Parser.AnyChar).rep )
      def TripleTail = R( TQ ~ "\"".rep )
      def SingleChars(allowInterp: Boolean) = R( (InterpIf(allowInterp) | "\\\"" | "\\\\" | !CharSets("\n\"") ~ Parser.AnyChar).rep )
      R {
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
