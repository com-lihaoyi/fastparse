package scalaparse

import acyclic.file
import syntax.{Identifiers, Key, Basic}

import scala.language.implicitConversions
import syntax.Basic._
import syntax.Identifiers
import fastparse._
trait Core extends syntax.Literals{
  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.

  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  val WS = P( (Basic.WSChars | Literals.Comment).rep )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  val WL = P( (Basic.WSChars | Literals.Comment | Basic.Newline).rep )


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
    def W(s: String) = P( WL ~ Key.W(s) )(s"`$s`")
    def O(s: String) = P( WL ~ Key.O(s) )(s"`$s`")
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
  val `*` = O("*")
  val `_*` = P( `_` ~ `*` )
  val `}` = P( Semis.? ~ "}" )
  val `{` = P( "{" ~ Semis.? )
  /**
   * helper printing function
   */

  val Id = P( WL ~ Identifiers.Id )
  val VarId = P( WL ~ Identifiers.VarId )
  val ExprLiteral = P( WL ~ Literals.Expr.Literal )
  val PatLiteral = P( WL ~ Literals.Pat.Literal )
  val Semi = P( WS ~ Basic.Semi )
  val Semis = P( Semi.rep(1) )
  val Newline = P( WL ~ Basic.Newline )

  val QualId = P( WL ~ Id.rep(1, sep = ".") )
  val Ids = P( Id.rep(1, sep = ",") )

  val NotNewline: P0 = P( &( WS ~ !Basic.Newline ) )
  val OneNLMax: P0 = {
    val ConsumeComments = P( (Basic.WSChars.? ~ Literals.Comment ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline )
  }
  /**
   * Sketchy way to whitelist a few suffixes that come after a . select;
   * apart from these and IDs, everything else is illegal
   */
  val PostDotCheck = P( WL ~ !(`super` | `this` | "{" | `_` | `type`) )
  val StableId: P0 = {
    val ClassQualifier = P( "[" ~ Id ~ "]" )
    val ThisSuper = P( `this` | `super` ~ ClassQualifier.? )
    val ThisPath = P( ThisSuper ~ ("." ~ PostDotCheck ~! Id).rep )
    val IdPath = P( Id ~ ("." ~ PostDotCheck ~! Id).rep ~ ("." ~ ThisPath).? )
    P( ThisPath | IdPath )
  }
}
