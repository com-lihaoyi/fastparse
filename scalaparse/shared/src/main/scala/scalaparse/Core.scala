package scalaparse

import acyclic.file
import syntax.{Key, Basic}

import scala.language.implicitConversions
import syntax.Identifiers
import fastparse.noApi._
trait Core extends syntax.Literals{
  import fastparse.noApi._
  val WhitespaceApi = new fastparse.WhitespaceApi.Wrapper(WL0)
  import WhitespaceApi._


  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.

  import Key._
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
  val BacktickId = P( WL ~ Identifiers.BacktickId )
  val ExprLiteral = P( WL ~ Literals.Expr.Literal )
  val PatLiteral = P( WL ~ Literals.Pat.Literal )

  val QualId = P( WL ~ Id.rep(1, sep = ".") )
  val Ids = P( Id.rep(1, sep = ",") )

  /**
   * Sketchy way to whitelist a few suffixes that come after a . select;
   * apart from these and IDs, everything else is illegal
   */
  val PostDotCheck: P0 = P( WL ~ !(`super` | `this` | "{" | `_` | `type`) )
  val StableId: P0 = {
    val ClassQualifier = P( "[" ~ Id ~ "]" )
    val ThisSuper = P( `this` | `super` ~ ClassQualifier.? )
    val ThisPath: P0 = P( ThisSuper ~ ("." ~ PostDotCheck ~/ Id).rep )
    val IdPath: P0 = P( Id ~ ("." ~ PostDotCheck ~/ (`this` | Id)).rep ~ ("." ~ ThisPath).? )
    P( ThisPath | IdPath )
  }
}
