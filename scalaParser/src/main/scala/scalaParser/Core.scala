package scalaParser
import acyclic.file

import scala.language.implicitConversions
import syntax.Basic._
import scalaParser.syntax.{Key, Basic, Identifiers}
import parsing._
trait Core extends syntax.Literals{
  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.
  type R0 = Parser0
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  val WS = R( (Basic.WSChar | Literals.Comment).rep )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  val WL = R( (Basic.WSChar | Literals.Comment | Basic.Newline).rep )


  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  private implicit def wspStr(s: String) = WL ~ s

  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object KeyWordOperators {
    def W(s: String) = R( WL ~ Key.W(s) )(s"`$s`")
    def O(s: String) = R( WL ~ Key.O(s) )(s"`$s`")
  }
  import KeyWordOperators._
  // Keywords that match themselves and nothing else
  val `=>` = O("=>") | O("⇒")
  val `<-` = O("<-") | O("←")
  val `:` = O(":")
  val `=` = O("=")
  val `@` = O("@")
  val `_` = W("_")
  val `this` = W("this")
  val `type` = W("type")
  val `val` = W("val")
  val `var` = W("var")
  val `def` = W("def")
  val `with` = W("with")
  val `package` = W("package")
  val `object` = W("object")
  val `class` = W("class")
  val `case` = W("case")
  val `trait` = W("trait")
  val `extends` = W("extends")
  val `implicit` = W("implicit")
  val `try` = W("try")
  val `new` = W("new")
  val `macro` = W("macro")
  val `import` = W("import")
  val `else` = W("else")
  val `super` = W("super")
  val `catch` = W("catch")
  val `finally` = W("finally")
  val `do` = W("do")
  val `yield` = W("yield")
  val `while` = W("while")
  val `<%` = O("<%")
  val `override` = W("override")
  val `#` = O("#")
  val `forSome` = W("forSome")
  val `for` = W("for")
  val `abstract` = W("abstract")
  val `throw` = W("throw")
  val `return` = W("return")
  val `lazy` = W("lazy")
  val `if` = W("if")
  val `match` = W("match")
  val `>:` = O(">:")
  val `<:` = O("<:")
  val `final` =  W("final")
  val `sealed` = W("sealed")
  val `private` = W("private")
  val `protected` = W("protected")

  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  val `_*` = R( `_` ~ "*" )
  val `}` = R( Semis.? ~ "}" )
  val `{` = R( "{" ~ Semis.? )
  /**
   * helper printing function
   */

  val Id = R( WL ~ Identifiers.Id )
  val VarId = R( WL ~ Identifiers.VarId )
  val Literal = R( WL ~ Literals.Literal )
  val Semi = R( WS ~ Basic.Semi )
  val Semis = R( Semi.rep1 )
  val Newline = R( WL ~ Basic.Newline )

  val QualId = R( WL ~ Id.rep1(".") )
  val Ids = R( Id.rep1(",") )

  val NotNewline: R0 = R( &( WS ~ !Basic.Newline ) )
  val OneNLMax: R0 = {
    val WSChar = R( Basic.WSChar.rep )
    val ConsumeComments = R( (WSChar ~ Literals.Comment ~ WSChar ~ Basic.Newline).rep )
    R( WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline )
  }
  val StableId: R0 = {
    val ClassQualifier = R( "[" ~ Id ~ "]" )
    val ThisSuper = R( `this` | `super` ~ ClassQualifier.? )
    R( (Id ~ ".").rep ~ ThisSuper ~ ("." ~ Id).rep | Id ~ ("." ~ Id).rep )
  }

}
