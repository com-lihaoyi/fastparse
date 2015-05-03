package scalaParser
package syntax
import acyclic.file
import parsing.Parser.CharsWhile
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
      def Thing = R( DecNum ~ Exp.? ~ FloatType.? )
      def Thing2 = R( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      R( "." ~ Thing | DecNum ~ Thing2 )
    }

    val Int = R( (HexNum | DecNum) ~ CharIn("Ll").? )

    val Bool = R( Key.W("true") | Key.W("false")  )

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    val CommentChunk = R( CharsWhile(!"/*".contains(_), min = 1) | MultilineComment | !"*/" ~ Parser.AnyChar )
    val MultilineComment: R0 = R( "/*" ~ CommentChunk.rep ~ "*/" )
    val LineComment = R( "//" ~ (CharsWhile(!"\n\r".contains(_), min = 1) | !Basic.Newline ~ Parser.AnyChar).rep ~ &(Basic.Newline | Parser.End) )
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

    class InterpCtx(interp: Option[R0]){
      val Literal = R( ("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null )
      def Interp = {
        "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ interp.get ~ WL ~ "}") | "$$"
      }

      val InterpIf = R( if(interp.isDefined) Interp else Parser.Fail )
      def TQ = R( "\"\"\"" )
      def TripleChars = R( (InterpIf | "\"".? ~ "\"".? ~ !"\"" ~ Parser.AnyChar).rep )
      def TripleTail = R( TQ ~ "\"".rep )
      def SingleChars = R( (InterpIf | "\\\"" | "\\\\" | !CharIn("\n\"") ~ Parser.AnyChar).rep )
      val String = {
        R {
          (Id ~ TQ ~ TripleChars ~ TripleTail) |
          (Id ~ "\"" ~ SingleChars  ~ "\"") |
          (TQ ~ NoInterp.TripleChars ~ TripleTail) |
          ("\"" ~ NoInterp.SingleChars ~ "\"")
        }
      }

    }
    object NoInterp extends InterpCtx(None)
    object Pat extends InterpCtx(Some(l.Pat))
    object Expr extends InterpCtx(Some(Block))


    def isPrintableChar(c: Char): Boolean = {
      val block = Character.UnicodeBlock.of(c)
      !Character.isISOControl(c) && !Character.isSurrogate(c) && block != null && block != Character.UnicodeBlock.SPECIALS
    }
  }
}
