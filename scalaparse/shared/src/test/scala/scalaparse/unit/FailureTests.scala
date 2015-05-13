package scalaparse.unit

import scalaparse.TestUtil
import utest._
import TestUtil._
object FailureTests extends TestSuite{
  val tests = TestSuite{

    * - checkNeg("package package", "(PkgBlock | PkgObj)", " package")

    * - checkNeg(
      """package torimatomeru
        |import a
        |import import
      """.stripMargin,
      expected = """(ThisPath | IdPath)""",
      found = " import"
    )

    * - checkNeg(
      """object O{
        |  for{
        |    x <- Nil
        |    if 1 ==
        |
        |    2
        |  } yield x
        |}
      """.stripMargin,
      expected = """ (Generator | Assign) """,
      found = "\n  } yiel"
    )
    * - checkNeg(
      """object O{
        | def from(n: Long, c: Int = 0): Int =
        |  if (n == 1) c + 1 else
        |}
      """.stripMargin,
      expected = """(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      found = ""
    )
    * - checkNeg(
      "import scala.util.{Failure, Success + 1}",
      expected = """ "}" """,
      found = "+ 1}"
    )
    * - checkNeg(
      """
        |object SyntaxTest extends TestSuite{
        |  def check[T]](input: String) = {
        |
        |  }
        |}
      """.stripMargin,
      expected = """ "}" """,
      found = "](input: S"
    )
    * - checkNeg(
      """
        |object SyntaxTest{
        |  a(
        |  throw 1
        |}
      """.stripMargin,
      expected = """ ")" """,
      found ="}"
    )
    * - checkNeg(
      """
        |object SyntaxTest {
        |  {
        |    thro   1
        |  }
        |}
      """.stripMargin,
      expected = """("}" | `case`)""",
      found ="   1"
    )
    * - checkNeg(
      """package scalatex
        |object ParserTests{
        |  new (1)
        |}
      """.stripMargin,
      expected = """(EarlyDefTmpl | NamedTmpl | TmplBody)""",
      found = " (1)"
    )
    * - checkNeg(
      """
        |object Moo{
        |  a
        |
        |  .
        |
        |  .
        |}
      """.stripMargin,
      expected = """(BacktickId | PlainId)""",
      found = "."
    )
    * - checkNeg(
      """
        |object Moo{
        | filename.asInstanceOf 10
        |}
      """.stripMargin,
      expected = """ "}" """,
      found = "10"
    )
    * - checkNeg(
      """
        |object C o w{
        |  ().mkString
        |
        |  1
        |}
      """.stripMargin,
      expected = "End",
      found = "o w{"
    )
    * - checkNeg(
      """
        |object O{
        | private[this] applyMacroFull = 1
        |}
      """.stripMargin,
      expected = "(Dcl | TraitDef | ClsDef | ObjDef)",
      found = " applyM"
    )
    * - checkNeg(
      """
        |object O{
        | private[this] def applyMacroFull(c: Context)
        |                      (expr: c.Expr[String],
        |                       runtimeErrors: Boolean,
        |                       debug: Boolean)
        |                      :  = {
        |                      }
        |}
      """.stripMargin,
      expected = "(`_` | PostfixType)",
      found = "  ="
    )
    * - checkNeg(
      """
        |object O{
        |  class 1 extends Exception
        |
        |  1
        |}
      """.stripMargin,
      expected = "(BacktickId | PlainId)",
      found = "1 extends"
    )
    * - checkNeg(
      """
        |package torimatomeru
        |
        |package syntax
        |
        |import org.parboiled2 _
        |
      """.stripMargin,
      expected = "End",
      found = "_"
    )
    * - checkNeg(
      """
        |object Foo{
        |  0 match {
        |    case A B => 0
        |  }
        |}
      """.stripMargin,
      expected = "(`=>` | `⇒`)",
      found = " B =>"
    )
    * - checkNeg(
      """
        |object Compiler{
        |
        |  def apply = {
        |    def rec = t match
        |      case 0 => 0
        |    }
        |
        |    rec(tree)
        |  }
        |}
        |
      """.stripMargin,
      expected = """ "{" """,
      found = "case 0 =>"
    )
    * - checkNeg(
      """
        |object O {
        |    A A(A(A(A(A(A(A())))))))
        |}
        |
      """.stripMargin,
      expected = """ "}" """,
      found = ")"
    )
    * - checkNeg(
      """
        |object O{
        |   A(A(A(A(A(A(A(A(A(A(A(A(A(A(A(A()))))))))))))))
        |}
      """.stripMargin,
      expected = """ ")" """,
      found = "}"
    )
    * - checkNeg(
      """
        |object L{
        |  a.b =
        |}
      """.stripMargin,
      expected = "(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)",
      found = "\n}"
    )
    * - checkNeg(
      """
        |object L{
        |  a b c
        |  d = 1
        |
      """.stripMargin,
      expected = """ "}" """,
      found = ""
    )
    //    Not sure how to properly put cuts on comments nodes
    //      * - checkNeg(
    //        """/*                     __                                               *\
    //          |**     ________ ___   / /  ___      __ ____  Scala.js CLI               **
    //          |**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
    //          |**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
    //          |** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
    //          |**                          |/____/                                     **
    //          |\*                                                                      *
    //          |
    //          |package scala.scalajs.cli
    //          |
    //        """.stripMargin,
    //        expected = """ "*/" """,
    //        found = ""
    //      )
    * - checkNeg(
      """
        |object O{
        |  for {
        |      a  <- b
        |      c <- d
        |  }
        |}
      """.stripMargin,
      expected = "(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)",
      found = "\n}"
    )
    * - checkNeg(
      """
        |object O{
        |  val jarFile = catch { case _: F => G }
        |}
      """.stripMargin,
      expected = "(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)",
      found = " catch {"
    )
    * - checkNeg(
      """
        |object F{
        |  func{ case _: F = fail }
        |}
      """.stripMargin,
      expected = "(`=>` | `⇒`)",
      found = " = fail"
    )
    * - checkNeg(
      """
        |object Foo{
        |    val a d // g
        |    val b = e // h
        |    val c = f
        |}
      """.stripMargin,
      expected = """ "}" """,
      found = "d // g"
    )
    * - checkNeg(
      """
        |object L{
        |  x match{
        |    case y.Y(z => z
        |  }
        |}
      """.stripMargin,
      expected = """("," ~ Pattern | ")")""",
      found = " => z"
    )
    * - checkNeg(
      """object K{
        |  val a:
        |    val c: D
        |  }
        |
        |  1
        |}
      """.stripMargin,
      expected = "(`_` | PostfixType)",
      found = "\n    val c"
    )
    * - checkNeg(
      """
        |object LOLS{
        |  def run(def apply() {}) {}
        |}
      """.stripMargin,
      expected = """ ")" """,
      found = "def apply"
    )
    * - checkNeg(
      """
        |object O{
        |  a =:= .c
        |}
      """.stripMargin,
      expected = """ "}" """,
      found = ".c"
    )
    * - checkNeg(
      """
        |object K{
        |  a(
        |    1:
        |  )
        |}
      """.stripMargin,
      expected = "(_* | AscriptionType | Annot.rep1)",
      found = "\n  )\n}"
    )
    * - checkNeg(
      """
        |object P{
        |  tree match {
        |    stats :+ expr  => 1
        |  }
        |}
      """.stripMargin,
      expected = """ "case" """,
      found = "stats :+ e"
    )
    * - checkNeg(
      """
        |object K
        |  val trueA = 1
        |}
      """.stripMargin,
      expected = "End",
      found = "val trueA"
    )
    * - checkNeg(
      """
        |object K{
        |  val null null cow = 1
        |}
      """.stripMargin,
      expected = """ "}" """,
      found = "null cow"
    )
    * - checkNeg(
      """
        |object K{
        |  val omg_+_+ = 1
        |}
      """.stripMargin,
      expected = """ "}" """,
      found = "_+ = 1"
    )
    * - checkNeg(
      """
        |object K{
        |  val + = 1
        |  var = 2
        |}
      """.stripMargin,
      expected = """(Binding ~ InfixPattern | InfixPattern | VarId)""",
      found = " = 2"
    )
    * - checkNeg(
      """
        |object O{
        |   {
        |    case b |  => 1
        |  }
        |}
      """.stripMargin,
      expected = """(TypePattern | BindPattern)""",
      found = "  => 1"
    )
    * - checkNeg(
      """
        |trait Basic {
        |  b match {
        |    case C case _ => false
        |  }
        |}
      """.stripMargin,
      expected = "(`=>` | `⇒`)",
      found = " case"
    )
    * - checkNeg(
      """trait Basic{
        |  a!.b
        |}
      """.stripMargin,
      expected = """ "}" """,
      found = ".b"
    )
    * - checkNeg(
      """
        |class Parser {
        |  {( => }
        |}
        |
      """.stripMargin,
      expected = """(Expr | ")")""",
      found = " => }"
    )
    * - checkNeg(
      """class C
        |package omg
        |;
      """.stripMargin,
      expected = """ "{" """,
      found = ";"
    )

    * - checkNeg(
      """
        |
        |object GenJSCode {
        |  code: @ 12
        |}
      """.stripMargin,
      expected = """("(" ~ Types.? ~ ")" | StableId ~ ("." ~ `type`).?)""",
      found = " 12"
    )

    * - checkNeg(
      """object B {
        |  { a: L = }
        |}
      """.stripMargin,
      expected = """ ("}" | `case`) """,
      found = " = }"
    )
    * - checkNeg(
      """object O{
        |    (i: Int => 10)
        |}
      """.stripMargin,
      expected = "(`_` | PostfixType)",
      found = " 10)"
    )
      * - checkNeg(
        """object GenJSCode{
          |  a.b.()
          |}
          |
        """.stripMargin,
        expected = "(BacktickId | PlainId)",
        found = "()"
      )
    * - checkNeg(
      """object GenJSCode{
        |  this.this.()
        |}
        |
      """.stripMargin,
      expected = "(BacktickId | PlainId)",
      found = "this"
    )
    * - checkNeg(
      """object GenJSCode{
        |  null: b.()
        |}
        |
      """.stripMargin,
      expected = "(BacktickId | PlainId)",
      found = "()"
    )
      * - checkNeg(
        """object K{
          |  private O
          |}
        """.stripMargin,
        expected = "(Dcl | TraitDef | ClsDef | ObjDef)",
        found = " O\n"
      )
      * - checkNeg(
        """object O{
          |  if eqeq &&
          |
          |    false  1
          |}
        """.stripMargin,
        expected = """ "(" """,
        found = "eqeq"
      )
      * - checkNeg(
        """
          |object O{
          |  for{
          |    x <- Nil map
          |
          |    (x => x)
          |  } yield x
          |}
        """.stripMargin,
        expected = """("," ~ Pattern | ")")""",
        found = " => x)"
      )
    * - checkNeg(
      """
        |object O{
        |  for{
        |    x <- Nil
        |    println(1)
        |  } yield x
        |}
      """.stripMargin,
      expected = """(Generator | Assign)""",
      found = "\n  } yield"
    )
      * - checkNeg(
        """
          |object O{
          |  for{
          |    x <- Nil
          |    if
          |
          |    1 == 2
          |  } yield
          |}
        """.stripMargin,
        expected = "(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)",
        found = "\n}"
      )
      * - checkNeg(
        """
          |object ScopedVar {
          |  def withScopedVars(ass: Seq[_, ]) = 1
          |}
          |
        """.stripMargin,
        expected = "(`_` | PostfixType)",
        found = " ]) = 1"
      )
      * - checkNeg(
        """
          |abstract class JSASTTest extends DirectTest {
          |  def show: this.type = )
          |}
          |
        """.stripMargin,
        expected = "(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)",
        found = " )\n"

      )
      * - checkNeg(
        """object Traversers {
          |  {
          |        1
          |        case foreach nil
          |  }
          |}
        """.stripMargin,
        expected = """ "}" """,
        found = "case for"
      )
      * - checkNeg(
        """object Utils {
          |  "\q"
          |}
          |
        """.stripMargin,
        expected = """(CharIn("btnfr'\\\"]") | OctalEscape | UnicodeEscape)""",
        found = "q"
      )
      * - checkNeg(
        """object Utils {
          |  "
          |  1
          |  "
          |}
          |
        """.stripMargin,
        expected = """ "\"" """,
        found = "\n"
      )
      val tq = "\"\"\""
      * - checkNeg(
        s"""
          |object Utils {
          |  $tq
          |  1
          |  "
          |}
          |
        """.stripMargin,
        expected = """ "\"\"\"" """,
        found = ""
      )
      * - checkNeg(
        """
          |object X{
          |  ''
          |}
          |
        """.stripMargin,
        expected = "(Char | Symbol)",
        found = "'\n"
      )

      // These two guys pass in Scalac, but I'm not gonna support it
      * - checkNeg(
        """
          |object X{
          |  (1.)
          |}
          |
        """.stripMargin,
        expected = "(BacktickId | PlainId)",
        found = ")"
      )
      * - checkNeg(
        s"""
           |object X{
           |  1..toString
           |}
        """.stripMargin,
        expected = "(BacktickId | PlainId)",
        found = ".toString"
      )
      * - checkNeg(
        s"""
           |object X{
           |  val x, = 1
           |}
        """.stripMargin,
        expected = "(Binding ~ InfixPattern | InfixPattern | VarId)",
        found = " = 1"
      )
    * - checkNeg(
      s"""
         |object X{
         |  val (x,) = 1
         |}
        """.stripMargin,
      expected = "(TypePattern | BindPattern)",
      found = ") = 1"
    )
    * - checkNeg(
      s"""
         |object X{ val (_:) = 1 }
        """.stripMargin,
      expected = "(AnnotType.rep1((`with` ~! Pass)) ~ Refinement.? | Refinement)",
      found = ") = 1"
    )
    * - checkNeg(
      s"""
         |import x.{y=>}
        """.stripMargin,
      expected = "(Id | `_`)",
      found = "}"
    )
    * - checkNeg(
      s"""
         |import x.y,
        """.stripMargin,
      expected = "(ThisPath | IdPath)",
      found = "\n"
    )
    * - checkNeg(
      s"""
         |object X{type T = A with}
        """.stripMargin,
      expected = """("(" ~ Types.? ~ ")" | StableId ~ ("." ~ `type`).?)""",
      found = "}"
    )
    * - checkNeg(
      s"""
         |object X{def f(x: Int, ) = 1}
        """.stripMargin,
      expected = """(BacktickId | PlainId)""",
      found = ") = 1"
    )
    * - checkNeg(
      s"""
         |object X{val x = (1, 2,)}
        """.stripMargin,
      expected = """(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      found = ")"
    )
    * - checkNeg(
      s"""
         |object X{f[A,]}
        """.stripMargin,
      expected = """(`_` | PostfixType)""",
      found = "]"
    )
    * - checkNeg(
      s"""
         |object X{def f[T <% A <%] = 1}
        """.stripMargin,
      expected = """(`_` | PostfixType)""",
      found = "]"
    )
    * - checkNeg(
      s"""
         |object X{def f[T, B,] = 1}
        """.stripMargin,
      expected = """(Id | `_`)""",
      found = "]"
    )
    * - checkNeg(
      s"""
         |object X{type T = F forSome }}
        """.stripMargin,
      expected = """ "{" """,
      found = "}}"
    )

    * - checkNeg(
      s"""
         |object X{def f(x: Int =) = 1}
        """.stripMargin,
      expected = """(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      found = ") = 1"
    )
    * - checkNeg(
      s"""
         |object X{type T = Int#}
        """.stripMargin,
      expected = """(BacktickId | PlainId)""",
      found = "}"
    )
    * - checkNeg(
      s"""
         |object X{type T = }
        """.stripMargin,
      expected = """(`_` | PostfixType)""",
      found = " }"
    )
    * - checkNeg(
      s"""
         |object X{type T[,] = A }
        """.stripMargin,
      expected = """(Id | `_`)""",
      found = ",]"
    )
    * - checkNeg(
      s"""
         |object X{type T <: }
        """.stripMargin,
      expected = """(`_` | PostfixType)""",
      found = " }"
    )
      * - checkNeg(
        """
          |object System {
          |  def a[@b T @f [@b V]] = 1
          |}
          |
        """.stripMargin,
        expected = """ "]" """,
        found = "@f"
      )
      * - checkNeg(
        """
          |@func(} object System {
          |
          |}
          |
        """.stripMargin,
        expected = """ ")" """,
        found = "}"
      )
      * - checkNeg(
        """
          |object System {
          |  def f[[a] = 1
          |}
          |
        """.stripMargin,
        expected = """(Id | `_`)""",
        found = "["
      )

  }
}
