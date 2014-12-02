package scalaParser
package syntax
import acyclic.file
import org.parboiled2._

trait Literals { self: Parser with Basic with Identifiers =>
  def Block: Rule0
  def WL: Rule0
  object Literals{
    import Basic._
    def Float = {
      def Thing = rule( Digit.+ ~ Exp.? ~ FloatType.? )
      def Thing2 = rule( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      rule( "." ~ Thing | Digit.+ ~ Thing2 )
    } 

    def Int = rule( (HexNum | DecNum) ~ anyOf("Ll").? )

    def Bool = rule( Key.W("true") | Key.W("false")  )

    def MultilineComment: Rule0 = rule( "/*" ~ (MultilineComment | !"*/" ~ ANY).* ~ "*/" )
    def Comment: Rule0 = rule(
      MultilineComment | "//" ~ (!Basic.Newline ~ ANY).* ~ &(Basic.Newline | EOI)
    )
    def Null = Key.W("null")
    def Literal = rule( ("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null )

    def EscapedChars = rule( '\\' ~ anyOf("btnfr'\\\"") )

    // Note that symbols can take on the same values as keywords!
    def Symbol = rule( ''' ~ (Identifiers.PlainId | Identifiers.Keywords) )

    def Char = rule {
      "'" ~ (UnicodeEscape | EscapedChars | !'\\' ~ CharPredicate.from(isPrintableChar)) ~ "'"
    }


    def pr(s: String) = rule( run(println(s"LOGGING $cursor: $s")) )
    def Interp = rule{
      "$" ~ Identifiers.PlainIdNoDollar | "${" ~ Block ~ WL ~ "}" | "$$"
    }
    def String = {
      import Identifiers.Id
      def InterpIf(b: Boolean) = if(b) rule(Interp) else rule(MISMATCH0)
      def TQ = rule( "\"\"\"" )
      def TripleChars(b: Boolean) = rule( (InterpIf(b) | '"'.? ~ '"'.? ~ noneOf("\"")).* )
      def TripleTail = rule( TQ ~ zeroOrMore('"') )
      def SingleChars(b: Boolean) = rule( (InterpIf(b) | "\\\"" | "\\\\" | noneOf("\n\"")).* )
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

