package scalaparse

import syntax.{Basic, Key}

import scala.language.implicitConversions
import syntax.Identifiers
import fastparse._
import fastparse._, ScalaWhitespace._

import scala.annotation.{switch, tailrec}
trait Core extends syntax.Literals{


  implicit class TrailingCommaOps[+T](p0: => P[T]) {
    def repTC[R](min: Int = 0, max: Int = Int.MaxValue, exactly: Int = -1)
                (implicit ev: fastparse.Implicits.Repeater[T, R],
                 ctx: P[_]): P[R] =
      p0.rep[R](min = min, sep = ",", max = max, exactly = exactly) ~ TrailingComma
  }
  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.

  import Key._
  // Keywords that match themselves and nothing else
  def `=>`[_: P] = (O("=>") | O("⇒")).opaque("\"=>\"")
  def `<-`[_: P] = O("<-") | O("←").opaque("\"<-\"")
  def `:`[_: P] = O(":")
  def `=`[_: P] = O("=")
  def `@`[_: P] = O("@")
  def `_`[_: P] = W("_")
  def `this`[_: P] = W("this")
  def `type`[_: P] = W("type")
  def `val`[_: P] = W("val")
  def `var`[_: P] = W("var")
  def `def`[_: P] = W("def")
  def `with`[_: P] = W("with")
  def `package`[_: P] = W("package")
  def `object`[_: P] = W("object")
  def `class`[_: P] = W("class")
  def `case`[_: P] = W("case")
  def `trait`[_: P] = W("trait")
  def `extends`[_: P] = W("extends")
  def `implicit`[_: P] = W("implicit")
  def `try`[_: P] = W("try")
  def `new`[_: P] = W("new")
  def `macro`[_: P] = W("macro")
  def `import`[_: P] = W("import")
  def `else`[_: P] = W("else")
  def `super`[_: P] = W("super")
  def `catch`[_: P] = W("catch")
  def `finally`[_: P] = W("finally")
  def `do`[_: P] = W("do")
  def `yield`[_: P] = W("yield")
  def `while`[_: P] = W("while")
  def `<%`[_: P] = O("<%")
  def `override`[_: P] = W("override")
  def `#`[_: P] = O("#")
  def `forSome`[_: P] = W("forSome")
  def `for`[_: P] = W("for")
  def `abstract`[_: P] = W("abstract")
  def `throw`[_: P] = W("throw")
  def `return`[_: P] = W("return")
  def `lazy`[_: P] = W("lazy")
  def `if`[_: P] = W("if")
  def `match`[_: P] = W("match")
  def `>:`[_: P] = O(">:")
  def `<:`[_: P] = O("<:")
  def `final`[_: P] =  W("final")
  def `sealed`[_: P] = W("sealed")
  def `private`[_: P] = W("private")
  def `protected`[_: P] = W("protected")


  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  def `*`[_: P] = O("*")
  def `_*`[_: P] = P( `_` ~ `*` )
  def `}`[_: P] = P( Semis.? ~ "}" )
  def `{`[_: P] = P( "{" ~ Semis.? )
  /**
   * helper printing function
   */

  def Id[_: P] = P( WL ~ Identifiers.Id )
  def VarId[_: P] = P( WL ~ Identifiers.VarId )
  def BacktickId[_: P] = P( WL ~ Identifiers.BacktickId )
  def ExprLiteral[_: P] = P( WL ~ Literals.Expr.Literal )
  def PatLiteral[_: P] = P( WL ~ Literals.Pat.Literal )

  def QualId[_: P] = P( WL ~ Id.rep(1, sep = ".") )
  def Ids[_: P] = P( Id.rep(1, sep = ",") )

  /**
   * Sketchy way to whitelist a few suffixes that come after a . select;
   * apart from these and IDs, everything else is illegal
   */
  def PostDotCheck[_: P]: P[Unit] = P( WL ~ !(`super` | `this` | "{" | `_` | `type`) )
  def ClassQualifier[_: P] = P( "[" ~ Id ~ "]" )
  def ThisSuper[_: P] = P( `this` | `super` ~ ClassQualifier.? )
  def ThisPath[_: P]: P[Unit] = P( ThisSuper ~ ("." ~ PostDotCheck ~/ Id).rep )
  def IdPath[_: P]: P[Unit] = P( Id ~ ("." ~ PostDotCheck ~/ (`this` | Id)).rep ~ ("." ~ ThisPath).? )
  def StableId[_: P]: P[Unit] = P( ThisPath | IdPath )
}
