package scalaparse.syntax

import acyclic.file
import fastparse.all._


import Identifiers._

trait Literals { l =>
  def Block: P0

  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  val WS = P( NoCut(NoTrace((Basic.WSChars | Literals.Comment).rep)) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  val WL0 = P( NoTrace((Basic.WSChars | Literals.Comment | Basic.Newline).rep) )(sourcecode.Name("WL"))
  val WL = P( NoCut(WL0) )

  val Semi = P( WS ~ Basic.Semi )
  val Semis = P( Semi.rep(1) ~ WS )
  val Newline = P( WL ~ Basic.Newline )

  val NotNewline: P0 = P( &( WS ~ !Basic.Newline ) )
  val OneNLMax: P0 = {
    val ConsumeComments = P( (Basic.WSChars.? ~ Literals.Comment ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( NoCut( WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline) )
  }
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
    val MultilineComment: P0 = P( "/*" ~/ CommentChunk.rep ~ "*/" )
    val SameLineCharChunks = P( CharsWhile(!"\n\r".contains(_))  | !Basic.Newline ~ AnyChar )
    val LineComment = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
    val Comment: P0 = P( MultilineComment | LineComment )

    val Null = Key.W("null")

    val OctalEscape = P( Digit ~ Digit.? ~ Digit.? )
    val Escape = P( "\\" ~/ (CharIn("""btnfr'\"]""") | OctalEscape | UnicodeEscape ) )

    // Note that symbols can take on the same values as keywords!
    val Symbol = P( Identifiers.PlainId | Identifiers.Keywords )

    val Char = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(CharPredicates.isPrintableChar)

      P( (Escape | PrintableChar) ~ "'" )
    }

    class InterpCtx(interp: Option[P0]){
      val Literal = P( ("-".? ~ (Float | Int)) | Bool | String | "'" ~/ (Char | Symbol) | Null )
      val Interp = interp match{
        case None => P ( Fail )
        case Some(p) => P( "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ p ~ WL ~ "}") | "$$" )
      }


      val TQ = P( "\"\"\"" )
      /**
       * Helper to quickly gobble up large chunks of un-interesting
       * characters. We break out conservatively, even if we don't know
       * it's a "real" escape sequence: worst come to worst it turns out
       * to be a dud and we go back into a CharsChunk next rep
       */
      val StringChars = P( CharsWhile(!"\n\"\\$".contains(_)) )
      val NonTripleQuoteChar = P( "\"" ~ "\"".? ~ !"\"" | CharIn("\\$\n") )
      val TripleChars = P( (StringChars | Interp | NonTripleQuoteChar).rep )
      val TripleTail = P( TQ ~ "\"".rep )
      def SingleChars(allowSlash: Boolean) = {
        val LiteralSlash = P( if(allowSlash) "\\" else Fail )
        val NonStringEnd = P( !CharIn("\n\"") ~ AnyChar )
        P( (StringChars | Interp | LiteralSlash | Escape | NonStringEnd ).rep )
      }
      val String = {
        P {
          (Id ~ TQ ~/ TripleChars ~ TripleTail) |
          (Id ~ "\"" ~/ SingleChars(true)  ~ "\"") |
          (TQ ~/ NoInterp.TripleChars ~ TripleTail) |
          ("\"" ~/ NoInterp.SingleChars(false) ~ "\"")
        }
      }

    }
    object NoInterp extends InterpCtx(None)
    object Pat extends InterpCtx(Some(l.Pattern))
    object Expr extends InterpCtx(Some(Block))


  }
}
