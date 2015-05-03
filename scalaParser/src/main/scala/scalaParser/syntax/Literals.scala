package scalaParser
package syntax
import acyclic.file
import parsing._
import Basic._
import Identifiers._

trait Literals { l =>
  def Block: R0
  def WL: R0
  def Pat: R0
  object Literals{
    import Basic._
    val Float = {
      def Thing = R( Digit.rep1 ~ Exp.? ~ FloatType.? )
      def Thing2 = R( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      R( "." ~ Thing | Digit.rep1 ~ Thing2 )
    }

    val Int = R( (HexNum | DecNum) ~ CharIn("Ll").? )

    val Bool = R( Key.W("true") | Key.W("false")  )

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    val MultilineComment: R0 = R( "/*" ~ (MultilineComment | !"*/" ~ Parser.AnyChar).rep ~ "*/" )
    val LineComment = R( "//" ~ (!Basic.Newline ~ Parser.AnyChar).rep ~ &(Basic.Newline | Parser.End) )
    val Comment: R0 = R( MultilineComment | LineComment )

    val Null = Key.W("null")


    val EscapedChars = R( "\\" ~ CharIn("""btnfr'\"]"""))

    // Note that symbols can take on the same values as keywords!
    val Symbol = R( "'" ~ (Identifiers.PlainId | Identifiers.Keywords) )

    val Char = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(isPrintableChar)

      R {
        "'" ~ (UnicodeEscape | EscapedChars | !"\\" ~ PrintableChar) ~ "'"
      }
    }

    class InterpCtx(interp: R0){
      val Literal = R( ("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null )
      val Interp = {
        "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ interp ~ WL ~ "}") | "$$"
      }
      val String = {
        import Identifiers.Id
        def InterpIf(allowInterp: Boolean) = R( if(allowInterp) Interp else Parser.Fail )
        def TQ = R( "\"\"\"" )
        def TripleChars(allowInterp: Boolean) = R( (InterpIf(allowInterp) | "\"".? ~ "\"".? ~ !"\"" ~ Parser.AnyChar).rep )
        def TripleTail = R( TQ ~ "\"".rep )
        def SingleChars(allowInterp: Boolean) = R( (InterpIf(allowInterp) | "\\\"" | "\\\\" | !CharIn("\n\"") ~ Parser.AnyChar).rep )
        R {
          (Id ~ TQ ~ TripleChars(allowInterp = true) ~ TripleTail) |
            (Id ~ "\"" ~ SingleChars(allowInterp = true) ~ "\"") |
            (TQ ~ TripleChars(allowInterp = false) ~ TripleTail) |
            ("\"" ~ SingleChars(allowInterp = false) ~ "\"")
        }
      }

    }
    object Pat extends InterpCtx(l.Pat)
    object Expr extends InterpCtx(Block)

    def isPrintableChar(c: Char): Boolean = {
      val block = Character.UnicodeBlock.of(c)
      !Character.isISOControl(c) && !Character.isSurrogate(c) && block != null && block != Character.UnicodeBlock.SPECIALS
    }
  }
}
