package scalaparse

import acyclic.file
import syntax.Basic
import fastparse.all._

import scala.language.implicitConversions

trait Xml extends Core {
  def Patterns: P0
  val XmlExpr = P( WL ~ Xml.XmlContent.rep(min = 1, sep = WL.?) )
  val XmlPattern = P( WL ~ Xml.ElemPattern )

  private[this] object Xml {
    val Element   = P( TagHeader ~/ ("/>" | ">" ~/ Content ~/ ETag ) ) // FIXME tag must be balanced
    val TagHeader = P( "<" ~ Name ~/ (WL ~ Attribute).rep ~ WL.? )
    val ETag      = P( "</" ~ Name ~ WL.? ~ ">" )

    val Attribute = P( Name ~/ Eq ~/ AttValue )
    val Eq        = P( WL.? ~ "=" ~ WL.? )
    val AttValue  = P(
      "\"" ~/ (CharQ | Reference).rep ~ "\"" |
        "'" ~/ (CharA | Reference).rep ~ "'" |
        ScalaExpr
    )

    val Content        = P( (CharData | Reference | ScalaExpr | XmlContent).rep )
    val XmlContent: P0 = P( Unparsed | CDSect | PI | Comment | Element )

    val ScalaExpr = P( "{" ~ WS ~ Block ~ WL ~ "}" )

    val Unparsed = P( UnpStart ~/ UnpData ~ UnpEnd )
    val UnpStart = P( "<xml:unparsed" ~/ (WL ~ Attribute).rep ~ WL.? ~ ">" )
    val UnpEnd   = P( "</xml:unparsed>" )
    val UnpData  = P( (!UnpEnd ~ Char).rep )

    val CDSect  = P( CDStart ~/ CData ~ CDEnd )
    val CDStart = P( "<![CDATA[" )
    val CData   = P( (!"]]>" ~ Char).rep )
    val CDEnd   = P( "]]>" )

    val Comment = P( "<!--" ~/ ComText ~ "-->" )
    val ComText = P( (!"-->" ~ Char).rep )

    val PI         = P( "<?" ~ Name ~ WL.? ~ PIProcText.? ~ "?>" )
    val PIProcText = P( (!"?>" ~ Char).rep )

    val Reference = P( EntityRef | CharRef )
    val EntityRef = P( "&" ~ Name ~/ ";" )
    val CharRef   = P( "&#" ~ Num ~ ";" | "&#x" ~ HexNum ~ ";" )
    val Num       = P( CharIn('0' to '9').rep )
    val HexNum    = P( CharIn('0' to '9', 'a' to 'f', 'A' to 'F').rep )

    val CharData = P( (CharB | "{{" | "}}").rep(1) )

    val Char   = P( AnyChar )
    val Char1  = P( !("<" | "&") ~ Char )
    val CharQ  = P( !"\"" ~ Char1 )
    val CharA  = P( !"'" ~ Char1 )
    val CharB  = P( !("{" | "}") ~ Char1 )

    val Name: P0  = P( NameStart ~ NameChar.rep ).!.filter(_.last != ':').opaque("Name").map(_ => Unit) // discard result
    val NameStart = P( CharPred(isNameStart) )
    val NameChar  = P( CharPred(isNameChar) )

    val ElemPattern: P0 = P( TagPHeader ~/ ("/>" | ">" ~/ ContentP ~/ ETag ) )
    val TagPHeader      = P( "<" ~ Name ~ WL.?  )

    val ContentP: P0  = P( ( CharDataP | ScalaPatterns | ElemPattern ).rep )
    val ScalaPatterns = P( "{" ~ Patterns ~ WL ~ "}" )
    val CharDataP     = P( "&" ~ CharData.? | CharData ) // matches weirdness of scalac parser on xml reference.

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