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
  def `=>`[$: P] = (O("=>") | O("⇒")).opaque("\"=>\"")
  def `<-`[$: P] = O("<-") | O("←").opaque("\"<-\"")
  def `:`[$: P] = O(":")
  def `=`[$: P] = O("=")
  def `@`[$: P] = O("@")
  //def `_`[$: P] = W("_")
  def Underscore[$: P] = W("_")
  def `this`[$: P] = W("this")
  def `type`[$: P] = W("type")
  def `val`[$: P] = W("val")
  def `var`[$: P] = W("var")
  def `def`[$: P] = W("def")
  def `with`[$: P] = W("with")
  def `package`[$: P] = W("package")
  def `object`[$: P] = W("object")
  def `class`[$: P] = W("class")
  def `case`[$: P] = W("case")
  def `trait`[$: P] = W("trait")
  def `extends`[$: P] = W("extends")
  def `implicit`[$: P] = W("implicit")
  def `try`[$: P] = W("try")
  def `new`[$: P] = W("new")
  def `macro`[$: P] = W("macro")
  def `import`[$: P] = W("import")
  def `else`[$: P] = W("else")
  def `super`[$: P] = W("super")
  def `catch`[$: P] = W("catch")
  def `finally`[$: P] = W("finally")
  def `do`[$: P] = W("do")
  def `yield`[$: P] = W("yield")
  def `while`[$: P] = W("while")
  def `<%`[$: P] = O("<%")
  def `override`[$: P] = W("override")
  def `#`[$: P] = O("#")
  def `forSome`[$: P] = W("forSome")
  def `for`[$: P] = W("for")
  def `abstract`[$: P] = W("abstract")
  def `throw`[$: P] = W("throw")
  def `return`[$: P] = W("return")
  def `lazy`[$: P] = W("lazy")
  def `if`[$: P] = W("if")
  def `match`[$: P] = W("match")
  def `>:`[$: P] = O(">:")
  def `<:`[$: P] = O("<:")
  def `final`[$: P] =  W("final")
  def `sealed`[$: P] = W("sealed")
  def `private`[$: P] = W("private")
  def `protected`[$: P] = W("protected")


  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  def `*`[$: P] = O("*")
  // def `_*`[$: P] = P( `_` ~ `*` )
  def `Underscore*`[$: P] = P( Underscore ~ `*` )
  def `}`[$: P] = P( Semis.? ~ "}" )
  def `{`[$: P] = P( "{" ~ Semis.? )
  /**
   * helper printing function
   */

  def Id[$: P] = P( WL ~ Identifiers.Id )
  def VarId[$: P] = P( WL ~ Identifiers.VarId )
  def BacktickId[$: P] = P( WL ~ Identifiers.BacktickId )
  def ExprLiteral[$: P] = P( WL ~ Literals.Expr.Literal )
  def PatLiteral[$: P] = P( WL ~ Literals.Pat.Literal )

  def QualId[$: P] = P( WL ~ Id.rep(1, sep = ".") )
  def Ids[$: P] = P( Id.rep(1, sep = ",") )

  /**
   * Sketchy way to whitelist a few suffixes that come after a . select;
   * apart from these and IDs, everything else is illegal
   */
  def PostDotCheck[$: P]: P[Unit] = P( WL ~ !(`super` | `this` | "{" | Underscore | `type`) )
  def ClassQualifier[$: P] = P( "[" ~ Id ~ "]" )
  def ThisSuper[$: P] = P( `this` | `super` ~ ClassQualifier.? )
  def ThisPath[$: P]: P[Unit] = P( ThisSuper ~ ("." ~ PostDotCheck ~/ Id).rep )
  def IdPath[$: P]: P[Unit] = P( Id ~ ("." ~ PostDotCheck ~/ (`this` | Id)).rep ~ ("." ~ ThisPath).? )
  def StableId[$: P]: P[Unit] = P( ThisPath | IdPath )
}
