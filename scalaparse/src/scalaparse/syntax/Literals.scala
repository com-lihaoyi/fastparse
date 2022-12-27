package scalaparse.syntax

import fastparse._

import NoWhitespace._
import Identifiers._

trait Literals { l =>
  def Block[_p: P]: P[Unit]

  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS[_p: P]: P[Unit] = P( NoTrace((Basic.WSChars | Literals.Comment).rep) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL0[_p: P]: P[Unit] = P( ScalaWhitespace.whitespace(P.current) )
  def WL[_p: P]: P[Unit] = P( NoCut(WL0) )

  def Semis[_p: P]: P[Unit] = P( NoTrace(NoCut(WS) ~ Basic.Semi.rep(1, NoCut(WS)) ~ NoCut(WS)) )
  def Newline[_p: P]: P[Unit] = P( WL ~ Basic.Newline )

  def NotNewline[_p: P]: P[Unit] = P( &( WS ~ !Basic.Newline ) )
  def OneNLMax[_p: P]: P[Unit] = {
    def ConsumeComments = P( (Basic.WSChars.? ~ NoTrace(Literals.Comment) ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( NoCut(NoTrace(WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline) ))
  }

  def TrailingComma[_p: P]: P[Unit] = P( ("," ~ WS ~ Basic.Newline).? )
  def Pattern[_p: P]: P[Unit]

  object Literals{
    import Basic._
    def Float[_p: P] = {
      def LeadingDotFloat = P( "." ~ DecNum ~ Exp.? ~ FloatType.? )
      def FloatSuffix = P( LeadingDotFloat | Exp ~ FloatType.? | Exp.? ~ FloatType )
      P( LeadingDotFloat | DecNum ~ FloatSuffix )
    }

    def Int[_p: P] = P( (HexNum | DecNum) ~ ("L" | "l").? )

    def Bool[_p: P] = P( Key.W("true") | Key.W("false")  )

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    def CommentChunk[_p: P] = P( CharsWhile(c => c != '/' && c != '*') | MultilineComment | !"*/" ~ AnyChar )
    def MultilineComment[_p: P]: P[Unit] = P( "/*" ~/ CommentChunk.rep ~ "*/" )
    def SameLineCharChunks[_p: P] = P( CharsWhile(c => c != '\n' && c != '\r')  | !Basic.Newline ~ AnyChar )
    def LineComment[_p: P] = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
    def Comment[_p: P]: P[Unit] = P( MultilineComment | LineComment )

    def Null[_p: P] = Key.W("null")

    def OctalEscape[_p: P] = P( Digit ~ Digit.? ~ Digit.? )
    def Escape[_p: P] = P( "\\" ~/ (CharIn("""btnfr'\\"]""") | OctalEscape | UnicodeEscape ) )

    // Note that symbols can take on the same values as keywords!
    def Symbol[_p: P] = P( Identifiers.PlainId | Identifiers.Keywords )

    def Char[_p: P] = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(CharPredicates.isPrintableChar)

      P( (Escape | PrintableChar) ~ "'" )
    }

    class InterpCtx(interp: Option[() => P[Unit]]) {
      def Literal[_p: P] = P( ("-".? ~ (Float | Int)) | Bool | String | "'" ~/ (Char | Symbol) | Null )
      def Interp[_p: P] = interp match{
        case None => P ( Fail )
        case Some(p) => P( "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ p() ~ WL ~ "}") | "$$" )
      }

      def TQ[_p: P] = P( "\"\"\"" )
      /**
       * Helper to quickly gobble up large chunks of un-interesting
       * characters. We break out conservatively, even if we don't know
       * it's a "real" escape sequence: worst come to worst it turns out
       * to be a dud and we go back into a CharsChunk next rep
       */
      def StringChars[_p: P] = P( CharsWhile(c => c != '\n' && c != '"' && c != '\\' && c != '$') )
      def NonTripleQuoteChar[_p: P] = P( "\"" ~ "\"".? ~ !"\"" | CharIn("\\\\$\n") )
      def TripleChars[_p: P] = P( (StringChars | Interp | NonTripleQuoteChar).rep )
      def TripleTail[_p: P] = P( TQ ~ "\"".rep )
      def SingleChars[_p: P](allowSlash: Boolean) = {
        def LiteralSlash = P( if(allowSlash) "\\" else Fail )
        def NonStringEnd = P( !CharIn("\n\"") ~ AnyChar )
        P( (StringChars | Interp | LiteralSlash | Escape | NonStringEnd ).rep )
      }
      def String[_p: P] = {
        P {
          Id.filter(_ => interp.isDefined) ~ (
            TQ ~/ TripleChars ~ TripleTail |
            "\"" ~/ SingleChars(true)  ~ "\""
          ) |
          TQ ~/ NoInterp.TripleChars ~ TripleTail |
          "\"" ~/ NoInterp.SingleChars(false) ~ "\""
        }
      }

    }
    def NoInterp[_p: P] = new InterpCtx(None)
    def Pat[_p: P] = new InterpCtx(Some(() => l.Pattern))
    def Expr[_p: P] = new InterpCtx(Some(() => Block))

  }
}
