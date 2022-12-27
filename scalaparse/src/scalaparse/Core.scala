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
      p0.rep[R, Int, Int](min = min, sep = ",", max = max, exactly = exactly) ~ TrailingComma
  }
  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.

  import Key._
  // Keywords that match themselves and nothing else
  def `=>`[_p: P] = (O("=>") | O("⇒")).opaque("\"=>\"")
  def `<-`[_p: P] = O("<-") | O("←").opaque("\"<-\"")
  def `:`[_p: P] = O(":")
  def `=`[_p: P] = O("=")
  def `@`[_p: P] = O("@")
  // `_` is not a valid Scala 3 identifier
  def `__`[_p: P] = W("_")
  def `this`[_p: P] = W("this")
  def `type`[_p: P] = W("type")
  def `val`[_p: P] = W("val")
  def `var`[_p: P] = W("var")
  def `def`[_p: P] = W("def")
  def `with`[_p: P] = W("with")
  def `package`[_p: P] = W("package")
  def `object`[_p: P] = W("object")
  def `class`[_p: P] = W("class")
  def `case`[_p: P] = W("case")
  def `trait`[_p: P] = W("trait")
  def `extends`[_p: P] = W("extends")
  def `implicit`[_p: P] = W("implicit")
  def `try`[_p: P] = W("try")
  def `new`[_p: P] = W("new")
  def `macro`[_p: P] = W("macro")
  def `import`[_p: P] = W("import")
  def `else`[_p: P] = W("else")
  def `super`[_p: P] = W("super")
  def `catch`[_p: P] = W("catch")
  def `finally`[_p: P] = W("finally")
  def `do`[_p: P] = W("do")
  def `yield`[_p: P] = W("yield")
  def `while`[_p: P] = W("while")
  def `<%`[_p: P] = O("<%")
  def `override`[_p: P] = W("override")
  def `#`[_p: P] = O("#")
  def `forSome`[_p: P] = W("forSome")
  def `for`[_p: P] = W("for")
  def `abstract`[_p: P] = W("abstract")
  def `throw`[_p: P] = W("throw")
  def `return`[_p: P] = W("return")
  def `lazy`[_p: P] = W("lazy")
  def `if`[_p: P] = W("if")
  def `match`[_p: P] = W("match")
  def `>:`[_p: P] = O(">:")
  def `<:`[_p: P] = O("<:")
  def `final`[_p: P] =  W("final")
  def `sealed`[_p: P] = W("sealed")
  def `private`[_p: P] = W("private")
  def `protected`[_p: P] = W("protected")


  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  def `*`[_p: P] = O("*")
  def `_*`[_p: P] = P( `__` ~ `*` )
  def `}`[_p: P] = P( Semis.? ~ "}" )
  def `{`[_p: P] = P( "{" ~ Semis.? )
  /**
   * helper printing function
   */

  def Id[_p: P] = P( WL ~ Identifiers.Id )
  def VarId[_p: P] = P( WL ~ Identifiers.VarId )
  def BacktickId[_p: P] = P( WL ~ Identifiers.BacktickId )
  def ExprLiteral[_p: P] = P( WL ~ Literals.Expr.Literal )
  def PatLiteral[_p: P] = P( WL ~ Literals.Pat.Literal )

  def QualId[_p: P] = P( WL ~ Id.rep(1, sep = ".") )
  def Ids[_p: P] = P( Id.rep(1, sep = ",") )

  /**
   * Sketchy way to whitelist a few suffixes that come after a . select;
   * apart from these and IDs, everything else is illegal
   */
  def PostDotCheck[_p: P]: P[Unit] = P( WL ~ !(`super` | `this` | "{" | `__` | `type`) )
  def ClassQualifier[_p: P] = P( "[" ~ Id ~ "]" )
  def ThisSuper[_p: P] = P( `this` | `super` ~ ClassQualifier.? )
  def ThisPath[_p: P]: P[Unit] = P( ThisSuper ~ ("." ~ PostDotCheck ~/ Id).rep )
  def IdPath[_p: P]: P[Unit] = P( Id ~ ("." ~ PostDotCheck ~/ (`this` | Id)).rep ~ ("." ~ ThisPath).? )
  def StableId[_p: P]: P[Unit] = P( ThisPath | IdPath )
}
