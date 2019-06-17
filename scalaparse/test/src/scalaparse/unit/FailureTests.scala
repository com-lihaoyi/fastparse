package scalaparse.unit

import scalaparse.TestUtil
import utest._
import TestUtil._
object FailureTests extends TestSuite{
  val tests = Tests {

    test - checkNeg(
      "package package",
      aggregate = """(QualId | PkgBlock | PkgObj)""",
      terminals = """("`" | var-id | chars-while(OpCharNotSlash, 1) | "/" | operator | plain-id | id | "case" | "object")""",
      found = "package"
    )

    test - checkNeg(
      """package torimatomeru
        |import a
        |import import
      """.stripMargin,
      aggregate = """(Semis ~ `package` | Semis ~ TopStat | ThisPath | IdPath)""",
      terminals = """("this" | "super" | "`" | var-id | chars-while(OpCharNotSlash, 1) | "/" | operator | plain-id | id)""",
      found = "import"
    )

    test - checkNeg(
      """object O{
        |  for{
        |    x <- Nil
        |    if 1 ==
        |
        |    2
        |  } yield x
        |}
      """.stripMargin,
      aggregate = """(Id | Generator | Assign)""",
      terminals = """("`" | char-pred(UpperChar) | char-pred(LowerChar) | var-id | chars-while(OpCharNotSlash, 1) | "/" | operator | plain-id | id | "<-" | "←" | "=")""",
      found = "} yield x"
    )
    test - checkNeg(
      """object O{
        |  type T = (A B)
        |}
      """.stripMargin,
      aggregate = """(NamedType | Refinement)""",
      terminals = """(chars-while(IdCharacter, 1) | [_] | [ \t] | "/*" | "//" | "(" | "-" | "." | [0-9] | "0x" | "true" | "false" | "`" | char-pred(UpperChar) | char-pred(LowerChar) | var-id | chars-while(OpCharNotSlash, 1) | "/" | operator | plain-id | id | filter | "\"\"\"" | "\"" | "'" | "null" | "this" | "super" | "_" | "{")""",
      found = ")"
    )
    test - checkNeg(
      """object O{
        | def from(n: Long, c: Int = 0): Int =
        |  if (n == 1) c + 1 else
        |}
      """.stripMargin,
      aggregate = """(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      terminals = null,
      found = ""
    )
    test - checkNeg(
      "import scala.util.{Failure, Success + 1}",
      aggregate = """("=>" | "," | "}")""",
      terminals = null,
      found = "+ 1}"
    )
    test - checkNeg(
      """
        |object SyntaxTest extends TestSuite{
        |  def check[T]](input: String) = {
        |
        |  }
        |}
      """.stripMargin,
      aggregate = """(FunArgs | `:` | Body | Semis | "}")""",
      terminals = null,
      found = "](input: S"
    )
    test - checkNeg(
      """
        |object SyntaxTest{
        |  a(
        |  throw 1
        |}
      """.stripMargin,
      aggregate = """("." | TypeArgs | ArgList | `_` | Id | "=>" | `=` | MatchAscriptionSuffix | "," | `:` | ")")""",
      terminals = null,
      found ="}"
    )
    test - checkNeg(
      """
        |object SyntaxTest {
        |  {
        |    thro   1
        |  }
        |}
      """.stripMargin,
      aggregate = """("=>" | `:` | "." | TypeArgs | ArgList | `_` | Id | `=` | MatchAscriptionSuffix | Semis | "}")""",
      terminals = null,
      found ="1\n"
    )
    test - checkNeg(
      """
        |object Moo{
        |  a
        |
        |  .
        |
        |  .
        |}
      """.stripMargin,
      aggregate = """(`this` | Id)""",
      terminals = null,
      found = "."
    )
    test - checkNeg(
      """
        |object Moo{
        | filename.asInstanceOf 10
        |}
      """.stripMargin,
      aggregate = """("." | TypeArgs | ArgList | `_` | Id | "=>" | `=` | MatchAscriptionSuffix | Semis | "}")""",
      terminals = null,
      found = "10"
    )
    test - checkNeg(
      """
        |object C o w{
        |  ().mkString
        |
        |  1
        |}
      """.stripMargin,
      aggregate = """(DefTmpl | end-of-input)""",
      terminals = null,
      found = "o w{"
    )
    test - checkNeg(
      """
        |object O{
        | private[this] applyMacroFull = 1
        |}
      """.stripMargin,
      aggregate = """(Mod | Dcl | TraitDef | ClsDef | ObjDef)""",
      terminals = null,
      found = "applyM"
    )
    test - checkNeg(
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
      aggregate = """("=>" | NamedType | Refinement)""",
      terminals = null,
      found = "= {"
    )
    test - checkNeg(
      """
        |object O{
        |  class 1 extends Exception
        |
        |  1
        |}
      """.stripMargin,
      aggregate = """id""",
      terminals = null,
      found = "1 extends"
    )
    test - checkNeg(
      """
        |package torimatomeru
        |
        |package syntax
        |
        |import org.parboiled2 _
        |
      """.stripMargin,
      aggregate = """(Semis ~ `package` | "." | "," | end-of-input)""",
      terminals = null,
      found = "_"
    )
    test - checkNeg(
      """
        |object Foo{
        |  0 match {
        |    case A B => 0
        |  }
        |}
      """.stripMargin,
      aggregate = """(XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId)""",
      terminals = null,
      found = "=> 0"
    )
    test - checkNeg(
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
      aggregate = """ "{" """,
      terminals = null,
      found = "case 0 =>"
    )
    test - checkNeg(
      """
        |object O {
        |    A A(A(A(A(A(A(A())))))))
        |}
        |
      """.stripMargin,
      aggregate = """(WL ~ "." | WL ~ TypeArgs | NotNewline ~ ArgList | `_` | InfixSuffix | PostFix | "=>" | `=` | MatchAscriptionSuffix | Semis | "}")""",
      terminals = null,
      found = ")"
    )
    test - checkNeg(
      """
        |object O{
        |   A(A(A(A(A(A(A(A(A(A(A(A(A(A(A(A()))))))))))))))
        |}
      """.stripMargin,
      aggregate = """("." | TypeArgs | ArgList | `_` | Id | "=>" | `=` | MatchAscriptionSuffix | "," | `:` | ")")""",
      terminals = null,
      found = "}"
    )
    test - checkNeg(
      """
        |object L{
        |  a.b =
        |}
      """.stripMargin,
      aggregate = """(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      terminals = null,
      found = "}"
    )
    test - checkNeg(
      """
        |object L{
        |  a b c
        |  d = 1
        |
      """.stripMargin,
      aggregate = """("." | TypeArgs | "=>" | `=` | MatchAscriptionSuffix | Semis | "}")""",
      terminals = null,
      found = ""
    )
    test - checkNeg(
      """/*
        |
        |package scala.scalajs.cli
        |
      """.stripMargin,
      aggregate = """ "*/" """,
      terminals = null,
      found = ""
    )
    test - checkNeg(
      """/*/*
        |*/
        |package scala.scalajs.cli
        |
      """.stripMargin,
      aggregate = """ "*/" """,
      terminals = null,
      found = ""
    )
    test - checkNeg(
      """
        |object O{
        |  for {
        |      a  <- b
        |      c <- d
        |  }
        |}
      """.stripMargin,
      aggregate = """(`yield` | If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      terminals = null,
      found = "}"
    )
    test - checkNeg(
      """
        |object O{
        |  val jarFile = catch { case _: F => G }
        |}
      """.stripMargin,
      aggregate = """(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      terminals = null,
      found = "catch {"
    )
    test - checkNeg(
      """
        |object F{
        |  func{ case _: F = fail }
        |}
      """.stripMargin,
      aggregate = """("." | TypeArgs | `#` | Annot | `with` | { | "|" | Guard | "=>")""",
      terminals = null,
      found = "= fail"
    )
    test - checkNeg(
      """
        |object Foo{
        |    val a d // g
        |    val b = e // h
        |    val c = f
        |}
      """.stripMargin,
      aggregate = """(XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId)""",
      terminals = null,
      found = "val b = e"
    )
    test - checkNeg(
      """
        |object L{
        |  x match{
        |    case y.Y(z => z
        |  }
        |}
      """.stripMargin,
      aggregate = """(`:` | `@` | "." | TypeArgs | TupleEx | Id | "|" | "," | ")")""",
      terminals = null,
      found = "=> z"
    )
    test - checkNeg(
      """object K{
        |  val a:
        |    val c: D
        |  }
        |
        |  1
        |}
      """.stripMargin,
      aggregate = """("=>" | NamedType | Refinement)""",
      terminals = null,
      found = "val c"
    )
    test - checkNeg(
      """
        |object LOLS{
        |  def run(def apply() {}) {}
        |}
      """.stripMargin,
      aggregate = """(`implicit` | Args | ")")""",
      terminals = null,
      found = "def apply"
    )
    test - checkNeg(
      """
        |object O{
        |  a =:= .c
        |}
      """.stripMargin,
      aggregate = """(TypeArgs | PrefixExpr | Newline | "=>" | `=` | MatchAscriptionSuffix | Semis | "}")""",
      terminals = null,
      found = ".c"
    )
    test - checkNeg(
      """
        |object K{
        |  a(
        |    1:
        |  )
        |}
      """.stripMargin,
      aggregate = """(_* | AscriptionType | Annot.rep(1))""",
      terminals = null,
      found = ")\n}"
    )
    test - checkNeg(
      """
        |object K{
        |  a[)
        |}
      """.stripMargin,
      aggregate = """(Type | "," | "]")""",
      terminals = null,
      found = ")"
    )
    test - checkNeg(
      """
        |object K{
        |  a[b)
        |}
      """.stripMargin,
      aggregate = """("." | TypeArgs | `#` | NLAnnot | `with` | Refinement | `*` | Id | "=>" | ExistentialClause | `>:` | `<:` | "," | "]")""",
      terminals = null,
      found = ")"
    )
    test - checkNeg(
      """
        |object P{
        |  tree match {
        |    stats :+ expr  => 1
        |  }
        |}
      """.stripMargin,
      aggregate = """ "case" """,
      terminals = null,
      found = "stats :+ e"
    )

    test - checkNeg(
      """
        |object K
        |  val trueA = 1
        |}
      """.stripMargin,
      aggregate = """(DefTmpl | Semis ~ TopStat | end-of-input)""",
      terminals = null,
      found = "val trueA"
    )
    test - checkNeg(
      """
        |object K{
        |  val null null cow = 1
        |}
      """.stripMargin,
      aggregate = """(Id | "," | `:` | `=` | Semis | "}")""",
      terminals = null,
      found = "null cow"
    )
    test - checkNeg(
      """
        |object K{
        |  val omg_+_+ = 1
        |}
      """.stripMargin,
      aggregate = """(`@` | TQ | "\"" | "." | TypeArgs | TupleEx | Id | "," | `:` | `=` | Semis | "}")""",
      terminals = null,
      found = "_+ = 1"
    )
    test - checkNeg(
      """
        |object K{
        |  val + = 1
        |  var = 2
        |}
      """.stripMargin,
      aggregate = """(Semis ~ TmplStat | Binding | InfixPattern | VarId)""",
      terminals = null,
      found = "= 2"
    )
    test - checkNeg(
      """
        |object O{
        |   {
        |    case b |  => 1
        |  }
        |}
      """.stripMargin,
      aggregate = """(XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId)""",
      terminals = null,
      found = "=> 1"
    )
    test - checkNeg(
      """
        |trait Basic {
        |  b match {
        |    case C case _ => false
        |  }
        |}
      """.stripMargin,
      aggregate = """(`@` | "." | TypeArgs | TupleEx | Id | "|" | Guard | "=>")""",
      terminals = null,
      found = "case"
    )
    test - checkNeg(
      """trait Basic{
        |  a!.b
        |}
      """.stripMargin,
      aggregate = """(TypeArgs | PrefixExpr | Newline | "=>" | `=` | MatchAscriptionSuffix | Semis | "}")""",
      terminals = null,
      found = ".b"
    )
    test - checkNeg(
      """
        |class Parser {
        |  {( => }
        |}
        |
      """.stripMargin,
      aggregate = """(BlockLambdaHead | Expr | "," | ")")""",
      terminals = null,
      found = "=> }"
    )
    test - checkNeg(
      """
        |class Parser([
        |
      """.stripMargin,
      aggregate = """(`implicit` | ClsArg | "," | ")")""",
      terminals = null,
      found = "["
    )
    test - checkNeg(
      """
        |class Parser{
        | @mog
        |}
      """.stripMargin,
      aggregate = """("." | TypeArgs | `#` | "(" | Annot | Mod | Dcl | TraitDef | ClsDef | ObjDef)""",
      terminals = null,
      found = "}"
    )
    test - checkNeg(
      """class C
        |package omg
        |;
      """.stripMargin,
      aggregate = """(Semis ~ TopStat | "{")""",
      terminals = null,
      found = ";"
    )

    test - checkNeg(
      """object B {
        |  { a: L = }
        |}
      """.stripMargin,
      aggregate = """("." | TypeArgs | `#` | Annot | `with` | { | `*` | Id | "=>" | BlockLambda | BlockStat | Semis | "}")""",
      terminals = null,
      found = "= }"
    )
      test - checkNeg(
        """object GenJSCode{
          |  a.b.()
          |}
          |
        """.stripMargin,
        aggregate = """(`this` | Id)""",
      terminals = null,
        found = "()"
      )
    test - checkNeg(
      """object GenJSCode{
        |  this.this.()
        |}
        |
      """.stripMargin,
      aggregate = """(PostDotCheck | id)""",
      terminals = null,
      found = "this"
    )
    test - checkNeg(
      """object GenJSCode{
        |  null: b.()
        |}
        |
      """.stripMargin,
      aggregate = """(`this` | Id)""",
      terminals = null,
      found = "()"
    )
      test - checkNeg(
        """object K{
          |  private O
          |}
        """.stripMargin,
        aggregate = """(AccessQualifier | Mod | Dcl | TraitDef | ClsDef | ObjDef)""",
      terminals = null,
        found = "O\n"
      )
      test - checkNeg(
        """object O{
          |  if eqeq &&
          |
          |    false  1
          |}
        """.stripMargin,
        aggregate = """ "(" """,
      terminals = null,
        found = "eqeq"
      )
      test - checkNeg(
        """
          |object O{
          |  for{
          |    x <- Nil map
          |
          |    (x => x)
          |  } yield x
          |}
        """.stripMargin,
        aggregate = """(`:` | `@` | "." | TypeArgs | TupleEx | Id | "|" | "," | ")")""",
      terminals = null,
        found = "=> x)"
      )
    test - checkNeg(
      """
        |object O{
        |  for{
        |    x <- Nil
        |    println(1)
        |  } yield x
        |}
      """.stripMargin,
      aggregate = """(Id | Generator | Assign)""",
      terminals = null,
      found = "} yield"
    )
    test - checkNeg(
      """
        |object O{
        |  for{
        |    x <- Nil
        |    {
        |  } yield x
        |}
      """.stripMargin,
      aggregate = """("." | TypeArgs | "=>" | `=` | MatchAscriptionSuffix | Guard | ";" | GenAssign | "}")""",
      terminals = null,
      found = "{\n"
    )
    test - checkNeg(
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
      aggregate = """(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      terminals = null,
      found = "}"
    )
    test - checkNeg(
      """
        |object ScopedVar {
        |  def withScopedVars(ass: Seq[_, ]) = 1
        |}
        |
      """.stripMargin,
      aggregate = """(TypeArgs | `#` | NLAnnot | `with` | Refinement | `*` | Id | "=>" | ExistentialClause | `>:` | `<:` | "," ~ Type | "," ~ WS ~ Newline | "]")""",
      terminals = null,
      found = ", ]"
    )
    test - checkNeg(
      """
        |abstract class JSASTTest extends DirectTest {
        |  def show: this.type = )
        |}
        |
      """.stripMargin,
      aggregate = """(`macro` | If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      terminals = null,
      found = ")\n"

    )
    test - checkNeg(
      """object Traversers {
        |  {
        |        1
        |        case foreach nil
        |  }
        |}
      """.stripMargin,
      aggregate = """(BlockLambda | BlockStat | Semis | "}")""",
      terminals = null,
      found = "case for"
    )
    test - checkNeg(
      """object Utils {
        |  "\q"
        |}
        |
      """.stripMargin,
      aggregate = """([btnfr'\\\\\"]] | OctalEscape | UnicodeEscape)""",
      terminals = null,
      found = "q"
    )
    test - checkNeg(
      """object Utils {
        |  "
        |  1
        |  "
        |}
        |
      """.stripMargin,
      aggregate = """(StringChars | Interp | LiteralSlash | Escape | NonStringEnd | "\"")""",
      terminals = null,
      found = "\n"
    )
    val tq = "\"\"\""
    test - checkNeg(
      s"""
        |object Utils {
        |  $tq
        |  1
        |  "
        |}
        |
      """.stripMargin,
      aggregate = """(StringChars | Interp | NonTripleQuoteChar | "\"\"\"")""",
      terminals = null,
      found = ""
    )
    test - checkNeg(
      """
        |object X{
        |  ''
        |}
        |
      """.stripMargin,
      aggregate = """(Char | Symbol)""",
      terminals = null,
      found = "'\n"
    )

    // These two guys pass in Scalac, but I'm not gonna support it
    test - checkNeg(
      """
        |object X{
        |  (1.)
        |}
        |
      """.stripMargin,
      aggregate = """(DecNum | id)""",
      terminals = null,
      found = ")"
    )
    test - checkNeg(
      s"""
         |object X{
         |  1..toString
         |}
      """.stripMargin,
      aggregate = """(DecNum | id)""",
      terminals = null,
      found = ".toString"
    )
    test - checkNeg(
      s"""
         |object X{
         |  val x, = 1
         |}
      """.stripMargin,
      aggregate = """(Binding | InfixPattern | VarId)""",
      terminals = null,
      found = "= 1"
    )
  test - checkNeg(
    s"""
       |object X{
       |  val (x,) = 1
       |}
      """.stripMargin,
    aggregate = """(`:` | `@` | TQ | "\"" | "." | TypeArgs | TupleEx | Id | "|" | "," ~ Pattern | "," ~ WS ~ Newline | ")")""",
      terminals = null,
    found = ",)"
  )
  test - checkNeg(
    s"""
       |object X{ val (_:) = 1 }
      """.stripMargin,
    aggregate = """(TypePat | NamedType | Refinement)""",
      terminals = null,
    found = ") = 1"
  )
  test - checkNeg(
    s"""
       |import x.{y=>}
      """.stripMargin,
    aggregate = """(Id | `_`)""",
      terminals = null,
    found = "}"
  )
  test - checkNeg(
    s"""
       |import x.y,
      """.stripMargin,
    aggregate = """(ThisPath | IdPath)""",
      terminals = null,
    found = ""
  )
  test - checkNeg(
    s"""
       |object X{type T = A with}
      """.stripMargin,
    aggregate = """(TupleType | Literal | TypeId | `_`)""",
      terminals = null,
    found = "}"
  )
  test - checkNeg(
    s"""
       |object X{def f(x: Int, ) = 1}
      """.stripMargin,
    aggregate = """("." | TypeArgs | `#` | NLAnnot | `with` | Refinement | `*` | Id | "=>" | ExistentialClause | `>:` | `<:` | `=` | "," ~ FunArg | "," ~ WS ~ Newline | ")")""",
      terminals = null,
    found = ", )"
  )
  test - checkNeg(
    s"""
       |object X{(2,)}
      """.stripMargin,
    aggregate = """(FloatSuffix | "L" | "l" | WL ~ "." | WL ~ TypeArgs | Pass ~ ArgList | `_` | InfixSuffix | PostFix | "=>" | `=` | MatchAscriptionSuffix | "," ~ Expr | "," ~ WS ~ Newline | ")")""",
      terminals = null,
    found = ",)"
  )
  test - checkNeg(
    s"""
       |object X{f[A,]}
      """.stripMargin,
    aggregate = """("." | TypeArgs | `#` | NLAnnot | `with` | Refinement | `*` | Id | "=>" | ExistentialClause | `>:` | `<:` | "," ~ Type | "," ~ WS ~ Newline | "]")""",
      terminals = null,
    found = ",]"
  )
  test - checkNeg(
    s"""
       |object X{def f[T <% A <%] = 1}
      """.stripMargin,
    aggregate = """("=>" | NamedType | Refinement)""",
      terminals = null,
    found = "]"
  )
  test - checkNeg(
    s"""
       |object X{def f[T, B,] = 1}
      """.stripMargin,
    aggregate = """(TypeArgList | `>:` | `<:` | `<%` | `:` | "," ~ Annot.rep ~ TypeArg | "," ~ WS ~ Newline | "]")""",
      terminals = null,
    found = ",]"
  )
  test - checkNeg(
    s"""
       |object X{type T = F forSome }}
      """.stripMargin,
    aggregate = """ "{" """,
      terminals = null,
    found = "}}"
  )

  test - checkNeg(
    s"""
       |object X{def f(x: Int =) = 1}
      """.stripMargin,
    aggregate = """(If | While | Try | DoWhile | For | Throw | Return | ImplicitLambda | SmallerExprOrLambda)""",
      terminals = null,
    found = ") = 1"
  )
  test - checkNeg(
    s"""
       |object X{type T = Int#}
      """.stripMargin,
    aggregate = """id""",
      terminals = null,
    found = "}"
  )
  test - checkNeg(
    s"""
       |object X{type T = }
      """.stripMargin,
    aggregate = """("=>" | NamedType | Refinement)""",
      terminals = null,
    found = "}\n"
  )
  test - checkNeg(
    s"""
       |object X{type T[,] = A }
      """.stripMargin,
    aggregate = """(Annot | [+\\-] | Id | `_`)""",
      terminals = null,
    found = ",]"
  )
  test - checkNeg(
    s"""
       |object X{type T <: }
      """.stripMargin,
    aggregate = """("=>" | NamedType | Refinement)""",
      terminals = null,
    found = "}\n"
  )
    test - checkNeg(
      """
        |object System {
        |  def a[@b T @f [@b V]] = 1
        |}
        |
      """.stripMargin,
      aggregate = """(TypeArgList | `>:` | `<:` | `<%` | `:` | "," | "]")""",
      terminals = null,
      found = "@f"
    )
    test - checkNeg(
      """
        |@func(} object System {
        |
        |}
        |
      """.stripMargin,
      aggregate = """(Exprs | "," | ")")""",
      terminals = null,
      found = "}"
    )
    test - checkNeg(
      """
        |object System {
        |  def f[[a] = 1
        |}
        |
      """.stripMargin,
      aggregate = """(Annot | Id | `_`)""",
      terminals = null,
      found = "["
    )

    test - checkNeg(
      s"""
        |object System {
        |  $tq """".stripMargin,
      aggregate = """("\"" | StringChars | Interp | NonTripleQuoteChar | "\"\"\"")""",
      terminals = null,
      found = ""
    )
    test - checkNeg(
      """
        |object System {
        |  e match { case <xml:unparsed><</xml:unparsed> => }
        |}
        |
      """.stripMargin,
      aggregate = """(CharDataP | ScalaPatterns | ElemPattern | "</")""",
      terminals = null,
      found = "<</xml:unp"
    )

    test - checkNeg(
      s"""object Foo{
         |  for(i <- Nil if x: Int => bar) 1
         |}
       """.stripMargin,
      aggregate = """(TQ | "\"" | "." | WL ~ "." | WL ~ TypeArgs | Pass ~ ArgList | `_` | InfixSuffix | PostFix | Enumerator | ")")""",
      terminals = null,
      found = ": Int"
    )
    test - checkNeg(
      s"""object Foo{; x: Int => x}""",
      aggregate = """("." | TypeArgs | `#` | Annot | `with` | { | `*` | Id | Semis | "}")""",
      terminals = null,
      found = "=> x"
    )
    test - checkNeg(
      s"""object Foo{ (i: Int => +i) }""",
      aggregate = """(NamedType | Refinement)""",
      terminals = null,
      found = ")"
    )
    test - checkNeg(
      """
        |object X {
        |  for{
        |    a <- List(1)
        |    ;
        |  } yield a
        |}""".stripMargin,
      aggregate = """(GenAssign | Guard)""",
      terminals = null,
      found = "} yield a"
    )
    test - checkNeg(
      """
        |object X {
        |  {
        |    val x = 1
        |    ;
        |    """.stripMargin,
      aggregate = """(BlockLambda | BlockStat | Semis | "}")""",
      terminals = null,
      found = ""
    )

  }
}
