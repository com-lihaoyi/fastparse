package scalaparse.syntax

import fastparse._

import NoWhitespace._
import Identifiers._

trait Literals { l =>
  def Block[$: P]: P[Unit]

  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS[$: P]: P[Unit] = P( NoTrace((Basic.WSChars | Literals.Comment).rep) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL0[$: P]: P[Unit] = P( ScalaWhitespace.whitespace(P.current) )
  def WL[$: P]: P[Unit] = P( NoCut(WL0) )

  def Semis[$: P]: P[Unit] = P( NoTrace(NoCut(WS) ~ Basic.Semi.rep(1, NoCut(WS)) ~ NoCut(WS)) )
  def Newline[$: P]: P[Unit] = P( WL ~ Basic.Newline )

  def NotNewline[$: P]: P[Unit] = P( &( WS ~ !Basic.Newline ) )
  def OneNLMax[$: P]: P[Unit] = {
    def ConsumeComments = P( (Basic.WSChars.? ~ NoTrace(Literals.Comment) ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( NoCut(NoTrace(WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline) ))
  }

  def TrailingComma[$: P]: P[Unit] = P( ("," ~ WS ~ Basic.Newline).? )
  def Pattern[$: P]: P[Unit]

  object Literals{
    import Basic._
    def Float[$: P] = {
      def LeadingDotFloat = P( "." ~ DecNum ~ Exp.? ~ FloatType.? )
      def FloatSuffix = P( LeadingDotFloat | Exp ~ FloatType.? | Exp.? ~ FloatType )
      P( LeadingDotFloat | DecNum ~ FloatSuffix )
    }

    def Int[$: P] = P( (HexNum | DecNum) ~ ("L" | "l").? )

    def Bool[$: P] = P( Key.W("true") | Key.W("false")  )

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    def CommentChunk[$: P] = P( CharsWhile(c => c != '/' && c != '*') | MultilineComment | !"*/" ~ AnyChar )
    def MultilineComment[$: P]: P[Unit] = P( "/*" ~/ CommentChunk.rep ~ "*/" )
    def SameLineCharChunks[$: P] = P( CharsWhile(c => c != '\n' && c != '\r')  | !Basic.Newline ~ AnyChar )
    def LineComment[$: P] = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
    def Comment[$: P]: P[Unit] = P( MultilineComment | LineComment )

    def Null[$: P] = Key.W("null")

    def OctalEscape[$: P] = P( Digit ~ Digit.? ~ Digit.? )
    def Escape[$: P] = P( "\\" ~/ (CharIn("""btnfr'\\"]""") | OctalEscape | UnicodeEscape ) ).log

    // Note that symbols can take on the same values as keywords!
    def Symbol[$: P] = P( Identifiers.PlainId | Identifiers.Keywords )

    def Char[$: P] = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(CharPredicates.isPrintableChar)

      P( (Escape | PrintableChar) ~ "'" )
    }

    class InterpCtx(interp: Option[() => P[Unit]]) {
      def Literal[$: P] = P( ("-".? ~ (Float | Int)) | Bool | String | "'" ~/ (Char | Symbol) | Null )
      def Interp[$: P] = interp match{
        case None => P ( Fail )
        case Some(p) => P( "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ p() ~ WL ~ "}") | "$$" )
      }

      def TQ[$: P] = P( "\"\"\"" )
      /**
       * Helper to quickly gobble up large chunks of un-interesting
       * characters. We break out conservatively, even if we don't know
       * it's a "real" escape sequence: worst come to worst it turns out
       * to be a dud and we go back into a CharsChunk next rep
       */
      def StringChars[$: P] = P( CharsWhile(c => c != '\n' && c != '"' && c != '\\' && c != '$') )
      def NonTripleQuoteChar[$: P] = P( "\"" ~ "\"".? ~ !"\"" | CharIn("\\\\$\n") )
      def TripleChars[$: P] = P( (StringChars | Interp | NonTripleQuoteChar).rep )
      def TripleTail[$: P] = P( TQ ~ "\"".rep )
      def SingleChars[$: P](allowSlash: Boolean) = {
        def LiteralSlash = P( if(allowSlash) "\\" else Fail )
        def NonStringEnd = P( !CharIn("\n\"") ~ AnyChar )
        P( (StringChars | Interp | LiteralSlash | Escape | NonStringEnd ).rep )
      }

      def String[$: P] = {
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

    def NoInterp[$: P] = new InterpCtx(None)
    def Pat[$: P] = new InterpCtx(Some(() => l.Pattern))
    def Expr[$: P] = new InterpCtx(Some(() => Block))
  }
}
