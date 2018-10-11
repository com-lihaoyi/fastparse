package scalaparse

import syntax.Basic
import fastparse._
import fastparse._

import scala.language.implicitConversions
import NoWhitespace._

trait Xml {
  def WL[_: P]: P0
  def WS[_: P]: P0
  def Block[_: P]: P0
  def Patterns[_: P]: P[Unit]
  def XmlExpr[_: P] = P( WL ~ Xml.XmlContent.rep(min = 1, sep = WL.?) )
  def XmlPattern[_: P] = P( WL ~ Xml.ElemPattern )

  private[this] object Xml {
    def Element[_: P] = P( TagHeader ~/ ("/>" | ">" ~/ Content ~/ ETag ) )
    def TagHeader[_: P] = P( "<" ~ Name ~/ (WL ~ Attribute).rep ~ WL.? )
    def ETag[_: P] = P( "</" ~ Name ~ WL.? ~ ">" )

    def Attribute[_: P] = P( Name ~ Eq ~/ AttValue )
    def Eq[_: P] = P( WL.? ~ "=" ~ WL.? )
    def AttValue[_: P] = P(
      "\"" ~/ (CharQ | Reference).rep ~ "\"" |
        "'" ~/ (CharA | Reference).rep ~ "'" |
        ScalaExpr
    )

    def Content[_: P]        = P( (CharData | Reference | ScalaExpr | XmlContent).rep )
    def XmlContent[_: P]: P[Unit] = P( Unparsed | CDSect | PI | Comment | Element )

    def ScalaExpr[_: P] = P( "{" ~ WS ~ Block ~ WL ~ "}" )

    def Unparsed[_: P] = P( UnpStart ~/ UnpData ~ UnpEnd )
    def UnpStart[_: P] = P( "<xml:unparsed" ~/ (WL ~ Attribute).rep ~ WL.? ~ ">" )
    def UnpEnd[_: P] = P( "</xml:unparsed>" )
    def UnpData[_: P] = P( (!UnpEnd ~ AnyChar).rep )

    def CDSect[_: P] = P( CDStart ~/ CData ~ CDEnd )
    def CDStart[_: P] = P( "<![CDATA[" )
    def CData[_: P] = P( (!"]]>" ~ Char).rep )
    def CDEnd[_: P] = P( "]]>" )

    def Comment[_: P] = P( "<!--" ~/ ComText ~ "-->" )
    def ComText[_: P] = P( (!"--" ~ Char).rep ~ ("-" ~ &("--")).? )

    def PI[_: P] = P( "<?" ~ PITarget ~ PIProcText.? ~ "?>" )
    def PITarget[_: P] = P( !(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~ Name )
    def PIProcText[_: P] = P( WL ~ (!"?>" ~ Char).rep )

    def Reference[_: P] = P( EntityRef | CharRef )
    def EntityRef[_: P] = P( "&" ~ Name ~/ ";" )
    def CharRef[_: P] = P( "&#" ~ Num ~/ ";" | "&#x" ~ HexNum ~/ ";" )
    def Num[_: P] = P( CharIn("0-9").rep )
    def HexNum[_: P] = P( CharIn("0-9a-fA-F").rep )

    def CharData[_: P] = P( (!"{" ~ Char1 | "{{").rep(1) )

    def Char[_: P] = P( AnyChar )
    def Char1[_: P] = P( !("<" | "&") ~ Char )
    def CharQ[_: P] = P( !"\"" ~ Char1 )
    def CharA[_: P] = P( !"'" ~ Char1 )

    def Name[_: P] = P( NameStart ~ NameChar.rep )
    def NameStart[_: P] = P( CharPred(isNameStart) ).opaque("NameStart")
    def NameChar[_: P] = P( CharPred(isNameChar) ).opaque("NameChar")

    def ElemPattern[_: P]: P[Unit] = P( TagPHeader ~/ ("/>" | ">" ~/ ContentP ~/ ETag ) )
    def TagPHeader[_: P] = P( "<" ~ Name ~ WL.?  )

    def ContentP[_: P]: P[Unit]  = P( ( CharDataP | ScalaPatterns | ElemPattern ).rep )
    def ScalaPatterns[_: P] = P( "{" ~ Patterns ~ WL ~ "}" )
    def CharDataP[_: P] = P( "&" ~ CharData.? | CharData ) // matches weirdness of scalac parser on xml reference.

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