package scalaparse

import syntax.Basic
import fastparse._
import fastparse._

import scala.language.implicitConversions
import NoWhitespace._

trait Xml {
  def WL[$: P]: P0
  def WS[$: P]: P0
  def Block[$: P]: P0
  def Patterns[$: P]: P[Unit]
  def XmlExpr[$: P] = P( WL ~ Xml.XmlContent.rep(min = 1, sep = WL.?) )
  def XmlPattern[$: P] = P( WL ~ Xml.ElemPattern )

  private[this] object Xml {
    def Element[$: P] = P( TagHeader ~/ ("/>" | ">" ~/ Content ~/ ETag ) )
    def TagHeader[$: P] = P( "<" ~ Name ~/ (WL ~ Attribute).rep ~ WL.? )
    def ETag[$: P] = P( "</" ~ Name ~ WL.? ~ ">" )

    def Attribute[$: P] = P( Name ~ Eq ~/ AttValue )
    def Eq[$: P] = P( WL.? ~ "=" ~ WL.? )
    def AttValue[$: P] = P(
      "\"" ~/ (CharQ | Reference).rep ~ "\"" |
        "'" ~/ (CharA | Reference).rep ~ "'" |
        ScalaExpr
    )

    def Content[$: P]        = P( (CharData | Reference | ScalaExpr | XmlContent).rep )
    def XmlContent[$: P]: P[Unit] = P( Unparsed | CDSect | PI | Comment | Element )

    def ScalaExpr[$: P] = P( "{" ~ WS ~ Block ~ WL ~ "}" )

    def Unparsed[$: P] = P( UnpStart ~/ UnpData ~ UnpEnd )
    def UnpStart[$: P] = P( "<xml:unparsed" ~/ (WL ~ Attribute).rep ~ WL.? ~ ">" )
    def UnpEnd[$: P] = P( "</xml:unparsed>" )
    def UnpData[$: P] = P( (!UnpEnd ~ AnyChar).rep )

    def CDSect[$: P] = P( CDStart ~/ CData ~ CDEnd )
    def CDStart[$: P] = P( "<![CDATA[" )
    def CData[$: P] = P( (!"]]>" ~ Char).rep )
    def CDEnd[$: P] = P( "]]>" )

    def Comment[$: P] = P( "<!--" ~/ ComText ~ "-->" )
    def ComText[$: P] = P( (!"--" ~ Char).rep ~ ("-" ~ &("--")).? )

    def PI[$: P] = P( "<?" ~ PITarget ~ PIProcText.? ~ "?>" )
    def PITarget[$: P] = P( !(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~ Name )
    def PIProcText[$: P] = P( WL ~ (!"?>" ~ Char).rep )

    def Reference[$: P] = P( EntityRef | CharRef )
    def EntityRef[$: P] = P( "&" ~ Name ~/ ";" )
    def CharRef[$: P] = P( "&#" ~ Num ~/ ";" | "&#x" ~ HexNum ~/ ";" )
    def Num[$: P] = P( CharIn("0-9").rep )
    def HexNum[$: P] = P( CharIn("0-9a-fA-F").rep )

    def CharData[$: P] = P( (!"{" ~ Char1 | "{{").rep(1) )

    def Char[$: P] = P( AnyChar )
    def Char1[$: P] = P( !("<" | "&") ~ Char )
    def CharQ[$: P] = P( !"\"" ~ Char1 )
    def CharA[$: P] = P( !"'" ~ Char1 )

    def Name[$: P] = P( NameStart ~ NameChar.rep )
    def NameStart[$: P] = P( CharPred(isNameStart) ).opaque("NameStart")
    def NameChar[$: P] = P( CharPred(isNameChar) ).opaque("NameChar")

    def ElemPattern[$: P]: P[Unit] = P( TagPHeader ~/ ("/>" | ">" ~/ ContentP ~/ ETag ) )
    def TagPHeader[$: P] = P( "<" ~ Name ~ WL.?  )

    def ContentP[$: P]: P[Unit]  = P( ( CharDataP | ScalaPatterns | ElemPattern ).rep )
    def ScalaPatterns[$: P] = P( "{" ~ Patterns ~ WL ~ "}" )
    def CharDataP[$: P] = P( "&" ~ CharData.? | CharData ) // matches weirdness of scalac parser on xml reference.

    //================================================================================
    // From `scala.xml.parsing.TokenTests`
    //================================================================================

    /**
     * {{{
     *  NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
     *             | CombiningChar | Extender
     *  }}}
     *  See [4] and Appendix B of XML 1.0 specification.
     */
    def isNameChar(ch: Char) = {
      import java.lang.Character._
      // The constants represent groups Mc, Me, Mn, Lm, and Nd.

      isNameStart(ch) || (getType(ch).toByte match {
        case COMBINING_SPACING_MARK |
          ENCLOSING_MARK | NON_SPACING_MARK |
          MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
        case _ => ".-:" contains ch
      })
    }

    /**
     * {{{
     *  NameStart ::= ( Letter | '_' )
     *  }}}
     *  where Letter means in one of the Unicode general
     *  categories `{ Ll, Lu, Lo, Lt, Nl }`.
     *
     *  We do not allow a name to start with `:`.
     *  See [3] and Appendix B of XML 1.0 specification
     */
    def isNameStart(ch: Char) = {
      import java.lang.Character._

      getType(ch).toByte match {
        case LOWERCASE_LETTER |
          UPPERCASE_LETTER | OTHER_LETTER |
          TITLECASE_LETTER | LETTER_NUMBER => true
        case _ => ch == '_'
      }
    }
  }
}