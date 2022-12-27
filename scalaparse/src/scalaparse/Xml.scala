package scalaparse

import syntax.Basic
import fastparse._
import fastparse._

import scala.language.implicitConversions
import NoWhitespace._

trait Xml {
  def WL[_p: P]: P0
  def WS[_p: P]: P0
  def Block[_p: P]: P0
  def Patterns[_p: P]: P[Unit]
  def XmlExpr[_p: P] = P( WL ~ Xml.XmlContent.rep(min = 1, sep = WL.?) )
  def XmlPattern[_p: P] = P( WL ~ Xml.ElemPattern )

  private[this] object Xml {
    def Element[_p: P] = P( TagHeader ~/ ("/>" | ">" ~/ Content ~/ ETag ) )
    def TagHeader[_p: P] = P( "<" ~ Name ~/ (WL ~ Attribute).rep ~ WL.? )
    def ETag[_p: P] = P( "</" ~ Name ~ WL.? ~ ">" )

    def Attribute[_p: P] = P( Name ~ Eq ~/ AttValue )
    def Eq[_p: P] = P( WL.? ~ "=" ~ WL.? )
    def AttValue[_p: P] = P(
      "\"" ~/ (CharQ | Reference).rep ~ "\"" |
        "'" ~/ (CharA | Reference).rep ~ "'" |
        ScalaExpr
    )

    def Content[_p: P]        = P( (CharData | Reference | ScalaExpr | XmlContent).rep )
    def XmlContent[_p: P]: P[Unit] = P( Unparsed | CDSect | PI | Comment | Element )

    def ScalaExpr[_p: P] = P( "{" ~ WS ~ Block ~ WL ~ "}" )

    def Unparsed[_p: P] = P( UnpStart ~/ UnpData ~ UnpEnd )
    def UnpStart[_p: P] = P( "<xml:unparsed" ~/ (WL ~ Attribute).rep ~ WL.? ~ ">" )
    def UnpEnd[_p: P] = P( "</xml:unparsed>" )
    def UnpData[_p: P] = P( (!UnpEnd ~ AnyChar).rep )

    def CDSect[_p: P] = P( CDStart ~/ CData ~ CDEnd )
    def CDStart[_p: P] = P( "<![CDATA[" )
    def CData[_p: P] = P( (!"]]>" ~ Char).rep )
    def CDEnd[_p: P] = P( "]]>" )

    def Comment[_p: P] = P( "<!--" ~/ ComText ~ "-->" )
    def ComText[_p: P] = P( (!"--" ~ Char).rep ~ ("-" ~ &("--")).? )

    def PI[_p: P] = P( "<?" ~ PITarget ~ PIProcText.? ~ "?>" )
    def PITarget[_p: P] = P( !(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~ Name )
    def PIProcText[_p: P] = P( WL ~ (!"?>" ~ Char).rep )

    def Reference[_p: P] = P( EntityRef | CharRef )
    def EntityRef[_p: P] = P( "&" ~ Name ~/ ";" )
    def CharRef[_p: P] = P( "&#" ~ Num ~/ ";" | "&#x" ~ HexNum ~/ ";" )
    def Num[_p: P] = P( CharIn("0-9").rep )
    def HexNum[_p: P] = P( CharIn("0-9a-fA-F").rep )

    def CharData[_p: P] = P( (!"{" ~ Char1 | "{{").rep(1) )

    def Char[_p: P] = P( AnyChar )
    def Char1[_p: P] = P( !("<" | "&") ~ Char )
    def CharQ[_p: P] = P( !"\"" ~ Char1 )
    def CharA[_p: P] = P( !"'" ~ Char1 )

    def Name[_p: P] = P( NameStart ~ NameChar.rep )
    def NameStart[_p: P] = P( CharPred(isNameStart) ).opaque("NameStart")
    def NameChar[_p: P] = P( CharPred(isNameChar) ).opaque("NameChar")

    def ElemPattern[_p: P]: P[Unit] = P( TagPHeader ~/ ("/>" | ">" ~/ ContentP ~/ ETag ) )
    def TagPHeader[_p: P] = P( "<" ~ Name ~ WL.?  )

    def ContentP[_p: P]: P[Unit]  = P( ( CharDataP | ScalaPatterns | ElemPattern ).rep )
    def ScalaPatterns[_p: P] = P( "{" ~ Patterns ~ WL ~ "}" )
    def CharDataP[_p: P] = P( "&" ~ CharData.? | CharData ) // matches weirdness of scalac parser on xml reference.

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