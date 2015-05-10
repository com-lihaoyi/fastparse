package scalaparse.syntax

import acyclic.file
import fastparse._
import Basic._
import Identifiers._

trait Literals { l =>
  def Block: P0
  def WL: P0
  def Pattern: P0
  object Literals{
    import Basic._
    val Float = {
      def Thing = P( DecNum ~ Exp.? ~ FloatType.? )
      def Thing2 = P( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      P( "." ~ Thing | DecNum ~ Thing2 )
    }

    val Int = P( (HexNum | DecNum) ~ CharIn("Ll").? )

    val Bool = P( Key.W("true") | Key.W("false")  )

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    val CommentChunk = P( CharsWhile(!"/*".contains(_)) | MultilineComment | !"*/" ~ AnyChar )
    val MultilineComment: P0 = P( "/*" ~ CommentChunk.rep ~ "*/" )
    val SameLineCharChunks = P( CharsWhile(!"\n\r".contains(_))  | !Basic.Newline ~ AnyChar )
    val LineComment = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
    val Comment: P0 = P( MultilineComment | LineComment )

    val Null = Key.W("null")

    val OctalEscape = P( Digit ~ Digit.? ~ Digit.? )
    val EscapedChars = P( "\\" ~! (CharIn("""btnfr'\"]""") | OctalEscape | UnicodeEscape ) )

    // Note that symbols can take on the same values as keywords!
    val Symbol = P( Identifiers.PlainId | Identifiers.Keywords )

    val Char = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(CharPredicates.isPrintableChar)

      P( (EscapedChars | PrintableChar) ~ "'" )
    }

    class InterpCtx(interp: Option[P0]){
      val Literal = P( ("-".? ~ (Float | Int)) | Bool | String | "'" ~! (Char | Symbol) | Null )
      val Interp = interp match{
        case None => Fail
        case Some(p) => "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ p ~ WL ~ "}") | "$$"
      }


      val TQ = P( "\"\"\"" )
      /**
       * Helper to quickly gobble up large chunks of un-interesting
       * characters. We break out conservatively, even if we don't know
       * it's a "real" escape sequence: worst come to worst it turns out
       * to be a dud and we go back into a CharsChunk next rep
       */
      val CharsChunk = CharsWhile(!"\n\"\\$".contains(_))
      val TripleChars = P( (CharsChunk | Interp | "\"".? ~ "\"".? ~ !"\"" ~ AnyChar).rep )
      val TripleTail = P( TQ ~ "\"".rep )
      val SingleChars = P( (CharsChunk | Interp | EscapedChars | !CharIn("\n\"") ~ AnyChar).rep )
      val String = {
        P {
          (Id ~ TQ ~! TripleChars ~ TripleTail) |
          (Id ~ "\"" ~! SingleChars  ~ "\"") |
          (TQ ~! NoInterp.TripleChars ~ TripleTail) |
          ("\"" ~! NoInterp.SingleChars ~ "\"")
        }
      }

    }
    object NoInterp extends InterpCtx(None)
    object Pat extends InterpCtx(Some(l.Pattern))
    object Expr extends InterpCtx(Some(Block))


  }
}
