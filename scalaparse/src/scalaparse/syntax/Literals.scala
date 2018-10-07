package scalaparse.syntax

import fasterparser._
import fasterparser.Parsing._


import Identifiers._

trait Literals { l =>
  def Block: P[Unit]

  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS[_: P]: P[Unit] = P( NoCut(NoTrace((Basic.WSChars | Literals.Comment).rep)) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL0[_: P]: P[Unit] = P( NoTrace((Basic.WSChars | Literals.Comment | Basic.Newline).rep) )(sourcecode.Name("WL"))
  def WL[_: P]: P[Unit] = P( NoCut(WL0) )

  def Semi[_: P]: P[Unit] = P( WS ~ Basic.Semi )
  def Semis[_: P]: P[Unit] = P( Semi.rep(1) ~ WS )
  def Newline[_: P]: P[Unit] = P( WL ~ Basic.Newline )

  def NotNewline[_: P]: P[Unit] = P( &( WS ~ !Basic.Newline ) )
  def OneNLMax[_: P]: P[Unit] = {
    def ConsumeComments[_: P] = P( (Basic.WSChars.? ~ Literals.Comment ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( NoCut( WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline) )
  }
  def TrailingComma[_: P]: P[Unit] = P( ("," ~ WS ~ Basic.Newline).? )
  def Pattern: P[Unit]
  object Literals{
    import Basic._
    def Float[_: P] = {
      def Thing = P( DecNum ~ Exp.? ~ FloatType.? )
      def Thing2 = P( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      P( "." ~ Thing | DecNum ~ Thing2 )
    }

    def Int[_: P] = P( (HexNum | DecNum) ~ ("L" | "l").? )

    def Bool[_: P] = P( Key.W("true") | Key.W("false")  )

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    def CommentChunk[_: P] = P( CharsWhile(c => c != '/' && c != '*') | MultilineComment | !"*/" ~ AnyChar )
    def MultilineComment[_: P]: P[Unit] = P( "/*" ~/ CommentChunk.rep ~ "*/" )
    def SameLineCharChunks[_: P] = P( CharsWhile(c => c != '\n' && c != '\r')  | !Basic.Newline ~ AnyChar )
    def LineComment[_: P] = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
    def Comment[_: P]: P[Unit] = P( MultilineComment | LineComment )

    val Null = Key.W("null")

    def OctalEscape[_: P] = P( Digit ~ Digit.? ~ Digit.? )
    def Escape[_: P] = P( "\\" ~/ (CharIn("""btnfr'\"]""") | OctalEscape | UnicodeEscape ) )

    // Note that symbols can take on the same values as keywords!
    def Symbol[_: P] = P( Identifiers.PlainId | Identifiers.Keywords )

    val Char = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(CharPredicates.isPrintableChar)

      P( (Escape | PrintableChar) ~ "'" )
    }

    class InterpCtx(interp: Option[P[Unit]]){
      def Literal[_: P] = P( ("-".? ~ (Float | Int)) | Bool | String | "'" ~/ (Char | Symbol) | Null )
      val Interp = interp match{
        case None => P ( Fail )
        case Some(p) => P( "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ p ~ WL ~ "}") | "$$" )
      }


      def TQ[_: P] = P( "\"\"\"" )
      /**
       * Helper to quickly gobble up large chunks of un-interesting
       * characters. We break out conservatively, even if we don't know
       * it's a "real" escape sequence: worst come to worst it turns out
       * to be a dud and we go back into a CharsChunk next rep
       */
      def StringChars[_: P] = P( CharsWhile(c => c != '\n' && c != '"' && c != '\\' && c != '$') )
      def NonTripleQuoteChar[_: P] = P( "\"" ~ "\"".? ~ !"\"" | CharIn("\\$\n") )
      def TripleChars[_: P] = P( (StringChars | Interp | NonTripleQuoteChar).rep )
      def TripleTail[_: P] = P( TQ ~ "\"".rep )
      def SingleChars(allowSlash: Boolean) = {
        def LiteralSlash[_: P] = P( if(allowSlash) "\\" else Fail )
        def NonStringEnd[_: P] = P( !CharIn("\n\"") ~ AnyChar )
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
