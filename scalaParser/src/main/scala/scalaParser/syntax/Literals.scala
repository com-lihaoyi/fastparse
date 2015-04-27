package scalaParser
package syntax
import acyclic.file
import parsing.Parsing.Parser.CharPredicate
import parsing.Parsing._
import Basic._
import Identifiers._

trait Literals {
  def Block: Rule0
  def WL: Rule0
  object Literals{
    import Basic._
    val Float = {
      def Thing = rule( Digit.rep1 ~ Exp.? ~ FloatType.? )
      def Thing2 = rule( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      rule( "." ~ Thing | Digit.rep1 ~ Thing2 )
    }

    val Int = rule( (HexNum | DecNum) ~ ("L"|"l").? )

    val Bool = rule( Key.W("true") | Key.W("false")  )

    val MultilineComment: Rule0 = rule( "/*" ~ (MultilineComment | !"*/" ~ Parser.Any).rep ~ "*/" )
    val Comment: Rule0 = rule(
      MultilineComment | "//" ~ (!Basic.Newline ~ Parser.Any).rep ~ &(Basic.Newline)
    )
    val Null = Key.W("null")
    val Literal = rule( ("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null )

    val EscapedChars = rule( '\\' ~ CharPredicate("""btnfr'\"]""".toSet))

    // Note that symbols can take on the same values as keywords!
    val Symbol = rule( ''' ~ (Identifiers.PlainId | Identifiers.Keywords) )

    val Char = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPredicate(isPrintableChar)

      rule {
        "'" ~ (UnicodeEscape | EscapedChars | !'\\' ~ PrintableChar) ~ "'"
      }
    }

    val Interp = rule{
      "$" ~ Identifiers.PlainIdNoDollar | "${" ~ Block ~ WL ~ "}" | "$$"
    }
    val String = {
      import Identifiers.Id
      def InterpIf(b: Boolean) = if(b) rule(Interp) else rule(Parser.Fail)
      def TQ = rule( "\"\"\"" )
      def TripleChars(b: Boolean) = rule( (InterpIf(b) | '"'.? ~ '"'.? ~ !'"').rep )
      def TripleTail = rule( TQ ~ '"'.rep )
      def SingleChars(b: Boolean) = rule( (InterpIf(b) | "\\\"" | "\\\\" | !("\n"|'"')).rep )
      rule {
        (Id ~ TQ ~ TripleChars(b = true) ~ TripleTail) |
        (Id ~ '"' ~ SingleChars(b = true) ~ '"') |
        (TQ ~ TripleChars(b = false) ~ TripleTail) |
        ('"' ~ SingleChars(b = false) ~ '"')
      }
    }

    def isPrintableChar(c: Char): Boolean = {
      val block = Character.UnicodeBlock.of(c)
      !Character.isISOControl(c) && !Character.isSurrogate(c) && block != null && block != Character.UnicodeBlock.SPECIALS
    }
  }
}
