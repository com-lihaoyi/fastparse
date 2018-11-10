package scalaparse.unit

import scalaparse.TestUtil
import utest._
import TestUtil._
object FailureTests extends TestSuite{
  val tests = Tests {

    * - checkNeg(
      "package package",
      aggregate = """(Id | PkgBlock | `case` | PkgObj)""",
      terminals = """("`" | var-id | chars-while(OpCharNotSlash, 1) | "/" | operator | plain-id | id | "case" | "object")""",
      found = "package"
    )

    * - checkNeg(
      """package torimatomeru
        |import a
        |import import
      """.stripMargin,
      aggregate = """(ThisPath | IdPath)""",
      terminals = """("this" | "super" | "`" | var-id | chars-while(OpCharNotSlash, 1) | "/" | operator | plain-id | id)""",
      found = "import"
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
      aggregate = """(Id | Generator | Assign)""",
      terminals = """(id | "<-" | "=")""",
      found = "} yield x"
    )
    * - checkNeg(
      """object O{
        |  type T = (A B)
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("(" | literal | "this" | "super" | id | "_" | "{")""",
      found = ")"
    )
    * - checkNeg(
      """object O{
        | def from(n: Long, c: Int = 0): Int =
        |  if (n == 1) c + 1 else
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """Expr""",
      found = ""
    )
    * - checkNeg(
      "import scala.util.{Failure, Success + 1}",
      aggregate = """""",
      terminals = """("=>" | "," | "}")""",
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
      aggregate = """""",
      terminals = """("(" | ":" | "=" | "{" | ";" | "}")""",
      found = "](input: S"
    )
    * - checkNeg(
      """
        |object SyntaxTest{
        |  a(
        |  throw 1
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "(" | "{" | "_" | id | "=>" | "=" | "match" | ":" | "," | ")")""",
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
      aggregate = """""",
      terminals = """("=>" | ":" | "." | "[" | "(" | "{" | "_" | id | "=" | "match" | ";" | "}")""",
      found ="1\n"
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
      aggregate = """""",
      terminals = """("this" | id)""",
      found = "."
    )
    * - checkNeg(
      """
        |object Moo{
        | filename.asInstanceOf 10
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "(" | "{" | "_" | id | "=>" | "=" | "match" | ":" | ";" | "}")""",
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
      aggregate = """""",
      terminals = """("extends" | "<:" | "{" | ";" | end-of-input)""",
      found = "o w{"
    )
    * - checkNeg(
      """
        |object O{
        | private[this] applyMacroFull = 1
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """(modifier | definition)""",
      found = "applyM"
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
      aggregate = """""",
      terminals = """("=>" | "(" | literal | "this" | "super" | id | "_" | "{")""",
      found = "= {"
    )
    * - checkNeg(
      """
        |object O{
        |  class 1 extends Exception
        |
        |  1
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """id""",
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
      aggregate = """""",
      terminals = """("." | "," | ";" | end-of-input)""",
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
      aggregate = """""",
      terminals = """SimplePattern""",
      found = "=> 0"
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
      aggregate = """""",
      terminals = """ "{" """,
      found = "case 0 =>"
    )
    * - checkNeg(
      """
        |object O {
        |    A A(A(A(A(A(A(A())))))))
        |}
        |
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "(" | "{" | "_" | id | "=>" | "=" | "match" | ":" | ";" | "}")""",
      found = ")"
    )
    * - checkNeg(
      """
        |object O{
        |   A(A(A(A(A(A(A(A(A(A(A(A(A(A(A(A()))))))))))))))
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "(" | "{" | "_" | id | "=>" | "=" | "match" | ":" | "," | ")")""",
      found = "}"
    )
    * - checkNeg(
      """
        |object L{
        |  a.b =
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = "Expr",
      found = "}"
    )
    * - checkNeg(
      """
        |object L{
        |  a b c
        |  d = 1
        |
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "=>" | "=" | "match" | ":" | ";" | "}")""",
      found = ""
    )
    * - checkNeg(
      """/*
        |
        |package scala.scalajs.cli
        |
      """.stripMargin,
      aggregate = """""",
      terminals = """ "*/" """,
      found = ""
    )
    * - checkNeg(
      """/*/*
        |*/
        |package scala.scalajs.cli
        |
      """.stripMargin,
      aggregate = """""",
      terminals = """ "*/" """,
      found = ""
    )
    * - checkNeg(
      """
        |object O{
        |  for {
        |      a  <- b
        |      c <- d
        |  }
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("yield" | Expr)""",
      found = "}"
    )
    * - checkNeg(
      """
        |object O{
        |  val jarFile = catch { case _: F => G }
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = "Expr",
      found = "catch {"
    )
    * - checkNeg(
      """
        |object F{
        |  func{ case _: F = fail }
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "#" | "@" | "with" | "{" | "|" | "if" | "=>")""",
      found = "= fail"
    )
    * - checkNeg(
      """
        |object Foo{
        |    val a d // g
        |    val b = e // h
        |    val c = f
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """SimplePattern""",
      found = "val b = e"
    )
    * - checkNeg(
      """
        |object L{
        |  x match{
        |    case y.Y(z => z
        |  }
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """(":" | "@" | "." | "[" | "(" | id | "|" | "," | ")")""",
      found = "=> z"
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
      aggregate = """""",
      terminals = """("=>" | "(" | literal | "this" | "super" | id | "_" | "{")""",
      found = "val c"
    )
    * - checkNeg(
      """
        |object LOLS{
        |  def run(def apply() {}) {}
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("implicit" | "@" | id | ")")""",
      found = "def apply"
    )
    * - checkNeg(
      """
        |object O{
        |  a =:= .c
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("[" | [\\-+!~] | SimpleExpr | "=>" | "=" | "match" | ":" | ";" | "}")""",
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
      aggregate = """""",
      terminals = """("_" | "=>" | "(" | literal | "this" | "super" | id | "{" | "@")""",
      found = ")\n}"
    )
    * - checkNeg(
      """
        |object K{
        |  a[)
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("=>" | "(" | literal | "this" | "super" | id | "_" | "{" | "," | "]")""",
      found = ")"
    )
    * - checkNeg(
      """
        |object K{
        |  a[b)
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("\"\"\"" | "\"" | "." | "[" | "#" | "@" | "with" | "{" | "*" | id | "=>" | "forSome" | ">:" | "<:" | "," | "]")""",
      found = ")"
    )
    * - checkNeg(
      """
        |object P{
        |  tree match {
        |    stats :+ expr  => 1
        |  }
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """ "case" """,
      found = "stats :+ e"
    )

    * - checkNeg(
      """
        |object K
        |  val trueA = 1
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """ ("extends" | "<:" | "{" | ";" | "package" | "import" | "@" | modifier | "trait" | "case" | "class" | "object" | end-of-input)""",
      found = "val trueA"
    )
    * - checkNeg(
      """
        |object K{
        |  val null null cow = 1
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """(id | "," | ":" | "=" | ";" | "}")""",
      found = "null cow"
    )
    * - checkNeg(
      """
        |object K{
        |  val omg_+_+ = 1
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("@" | "\"\"\"" | "\"" | "." | "[" | "(" | id | "," | ":" | "=" | ";" | "}")""",
      found = "_+ = 1"
    )
    * - checkNeg(
      """
        |object K{
        |  val + = 1
        |  var = 2
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """(id | "_" | SimplePattern | var-id)""",
      found = "= 2"
    )
    * - checkNeg(
      """
        |object O{
        |   {
        |    case b |  => 1
        |  }
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """SimplePattern""",
      found = "=> 1"
    )
    * - checkNeg(
      """
        |trait Basic {
        |  b match {
        |    case C case _ => false
        |  }
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("@" | "." | "[" | "(" | id | "|" | "if" | "=>")""",
      found = "case"
    )
    * - checkNeg(
      """trait Basic{
        |  a!.b
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("[" | [\\-+!~] | SimpleExpr | "=>" | "=" | "match" | ":" | ";" | "}")""",
      found = ".b"
    )
    * - checkNeg(
      """
        |class Parser {
        |  {( => }
        |}
        |
      """.stripMargin,
      aggregate = """""",
      terminals = """("(" | "this" | id | "_" | Expr | "," | ")")""",
      found = "=> }"
    )
    * - checkNeg(
      """
        |class Parser([
        |
      """.stripMargin,
      aggregate = """""",
      terminals = """("implicit" | "@" | modifier | "val" | "var" | id | "," | ")")""",
      found = "["
    )
    * - checkNeg(
      """
        |class Parser{
        | @mog
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "#" | "(" | "@" | modifier | definition)""",
      found = "}"
    )
    * - checkNeg(
      """class C
        |package omg
        |;
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "{")""",
      found = ";"
    )

    * - checkNeg(
      """object B {
        |  { a: L = }
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "#" | "@" | "with" | "{" | "*" | id | "=>" | "(" | "this" | "_" | "import" | local-modifier | definition | Expr | ";" | "}")""",
      found = "= }"
    )
      * - checkNeg(
        """object GenJSCode{
          |  a.b.()
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """("this" | id)""",
        found = "()"
      )
    * - checkNeg(
      """object GenJSCode{
        |  this.this.()
        |}
        |
      """.stripMargin,
      aggregate = """""",
      terminals = """(!`super` | "this" ~ !LetterDigitDollarUnderscore | id)""",
      found = "this"
    )
    * - checkNeg(
      """object GenJSCode{
        |  null: b.()
        |}
        |
      """.stripMargin,
      aggregate = """""",
      terminals = """("this" | id)""",
      found = "()"
    )
      * - checkNeg(
        """object K{
          |  private O
          |}
        """.stripMargin,
        aggregate = """""",
      terminals = """("[" | modifier | definition)""",
        found = "O\n"
      )
      * - checkNeg(
        """object O{
          |  if eqeq &&
          |
          |    false  1
          |}
        """.stripMargin,
        aggregate = """""",
      terminals = """ "(" """,
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
        aggregate = """""",
      terminals = """(":" | "@" | "." | "[" | "(" | id | "|" | "," | ")")""",
        found = "=> x)"
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
      aggregate = """""",
      terminals = """(id | "<-" | "=")""",
      found = "} yield"
    )
    * - checkNeg(
      """
        |object O{
        |  for{
        |    x <- Nil
        |    {
        |  } yield x
        |}
      """.stripMargin,
      aggregate = """""",
      terminals = """("." | "[" | "=>" | "=" | "match" | ":" | "if" | ";" | "val" | "_" | "`" | var-id | id | SimplePattern | "}")""",
      found = "{\n"
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
        aggregate = """""",
      terminals = "Expr",
        found = "}"
      )
      * - checkNeg(
        """
          |object ScopedVar {
          |  def withScopedVars(ass: Seq[_, ]) = 1
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """("[" | "#" | "@" | "with" | "{" | "*" | id | "=>" | "forSome" | ">:" | "<:" | literal | "]")""",
        found = ", ]"
      )
      * - checkNeg(
        """
          |abstract class JSASTTest extends DirectTest {
          |  def show: this.type = )
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """("macro" | Expr)""",
        found = ")\n"

      )
      * - checkNeg(
        """object Traversers {
          |  {
          |        1
          |        case foreach nil
          |  }
          |}
        """.stripMargin,
        aggregate = """""",
      terminals = """("." | "[" | "=>" | "=" | "match" | ":" | ";" | "(" | "this" | id | "_" | "import" | "@" | local-modifier | definition | Expr | "}")""",
        found = "case for"
      )
      * - checkNeg(
        """object Utils {
          |  "\q"
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """([btnfr'\\\\\"]] | [0-9] | "u")""",
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
        aggregate = """""",
      terminals = """(chars-while(1) | "\\" | ![\n\"] | "\"")""",
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
        aggregate = """""",
      terminals = """(chars-while(1) | "\"" | [\\\\$\n] | "\"\"\"")""",
        found = ""
      )
      * - checkNeg(
        """
          |object X{
          |  ''
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """("\\" | Char | plain-id | AlphabetKeywords | SymbolicKeywords)""",
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
        aggregate = """""",
      terminals = """([0-9] | id)""",
        found = ")"
      )
      * - checkNeg(
        s"""
           |object X{
           |  1..toString
           |}
        """.stripMargin,
        aggregate = """""",
      terminals = """([0-9] | id)""",
        found = ".toString"
      )
      * - checkNeg(
        s"""
           |object X{
           |  val x, = 1
           |}
        """.stripMargin,
        aggregate = """""",
      terminals = """(id | "_" | SimplePattern | var-id)""",
        found = "= 1"
      )
    * - checkNeg(
      s"""
         |object X{
         |  val (x,) = 1
         |}
        """.stripMargin,
      aggregate = """""",
      terminals = """(":" | "@" | "\"\"\"" | "\"" | "." | "[" | "(" | id | "|" | SimplePattern | ")")""",
      found = ",)"
    )
    * - checkNeg(
      s"""
         |object X{ val (_:) = 1 }
        """.stripMargin,
      aggregate = """""",
      terminals = """("(" | literal | "this" | "super" | id | "_" | "{")""",
      found = ") = 1"
    )
    * - checkNeg(
      s"""
         |import x.{y=>}
        """.stripMargin,
      aggregate = """""",
      terminals = """(id | "_")""",
      found = "}"
    )
    * - checkNeg(
      s"""
         |import x.y,
        """.stripMargin,
      aggregate = """""",
      terminals = """("this" | "super" | id)""",
      found = ""
    )
    * - checkNeg(
      s"""
         |object X{type T = A with}
        """.stripMargin,
      aggregate = """""",
      terminals = """("(" | literal | "this" | "super" | id | "_")""",
      found = "}"
    )
    * - checkNeg(
      s"""
         |object X{def f(x: Int, ) = 1}
        """.stripMargin,
      aggregate = """""",
      terminals = """("\"\"\"" | "\"" | "." | "[" | "#" | "@" | "with" | "{" | "*" | id | "=>" | "forSome" | ">:" | "<:" | "=" | ")")""",
      found = ", )"
    )
    * - checkNeg(
      s"""
         |object X{val x = (1, 2,)}
        """.stripMargin,
      aggregate = """""",
      terminals = """("." | [Ee] | [fFdD] | "L" | "l" | "[" | "(" | "{" | "_" | id | "=>" | "=" | "match" | ":" | Expr | ")")""",
      found = ",)"
    )
    * - checkNeg(
      s"""
         |object X{f[A,]}
        """.stripMargin,
      aggregate = """""",
      terminals = """("\"\"\"" | "\"" | "." | "[" | "#" | "@" | "with" | "{" | "*" | id | "=>" | "forSome" | ">:" | "<:" | literal | "]")""",
      found = ",]"
    )
    * - checkNeg(
      s"""
         |object X{def f[T <% A <%] = 1}
        """.stripMargin,
      aggregate = """""",
      terminals = """("=>" | "(" | literal | "this" | "super" | id | "_" | "{")""",
      found = "]"
    )
    * - checkNeg(
      s"""
         |object X{def f[T, B,] = 1}
        """.stripMargin,
      aggregate = """""",
      terminals = """("[" | ">:" | "<:" | "<%" | ":" | "]")""" ,
      found = ",]"
    )
    * - checkNeg(
      s"""
         |object X{type T = F forSome }}
        """.stripMargin,
      aggregate = """""",
      terminals = """ "{" """,
      found = "}}"
    )

    * - checkNeg(
      s"""
         |object X{def f(x: Int =) = 1}
        """.stripMargin,
      aggregate = """""",
      terminals = """Expr""",
      found = ") = 1"
    )
    * - checkNeg(
      s"""
         |object X{type T = Int#}
        """.stripMargin,
      aggregate = """""",
      terminals = """id""",
      found = "}"
    )
    * - checkNeg(
      s"""
         |object X{type T = }
        """.stripMargin,
      aggregate = """""",
      terminals = """("=>" | "(" | literal | "this" | "super" | id | "_" | "{")""",
      found = "}\n"
    )
    * - checkNeg(
      s"""
         |object X{type T[,] = A }
        """.stripMargin,
      aggregate = """""",
      terminals = """("@" | [+\\-] | id | "_")""" ,
      found = ",]"
    )
    * - checkNeg(
      s"""
         |object X{type T <: }
        """.stripMargin,
      aggregate = """""",
      terminals = """("=>" | "(" | literal | "this" | "super" | id | "_" | "{")""",
      found = "}\n"
    )
      * - checkNeg(
        """
          |object System {
          |  def a[@b T @f [@b V]] = 1
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """("[" | ">:" | "<:" | "<%" | ":" | "," | "]")""",
        found = "@f"
      )
      * - checkNeg(
        """
          |@func(} object System {
          |
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """(Expr | "," | ")")""",
        found = "}"
      )
      * - checkNeg(
        """
          |object System {
          |  def f[[a] = 1
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """("@" | id | "_")""",
        found = "["
      )

      * - checkNeg(
        s"""
          |object System {
          |  $tq """".stripMargin,
        aggregate = """""",
      terminals = """("\"" | chars-while(1) | [\\\\$\n] | "\"\"\"")""",
        found = ""
      )
      * - checkNeg(
        """
          |object System {
          |  e match { case <xml:unparsed><</xml:unparsed> => }
          |}
          |
        """.stripMargin,
        aggregate = """""",
      terminals = """("&" | !"<" | "{{" | "{" | CharDataP | ScalaPatterns | ElemPattern.rep | "</")""",
        found = "<</xml:unp"
      )

      * - checkNeg(
        s"""object Foo{
           |  for(i <- Nil if x: Int => bar) 1
           |}
         """.stripMargin,
        aggregate = """""",
      terminals = """("\"\"\"" | "\"" | "." | "[" | "(" | "{" | "_" | id | ";" | "val" | "`" | var-id | SimplePattern | "if" | ")")""",
        found = ": Int"
      )
      * - checkNeg(
        s"""object Foo{; x: Int => x}""",
        aggregate = """""",
      terminals = """("." | "[" | "#" | "@" | "with" | "{" | "*" | id | ";" | "}")""",
        found = "=> x"
      )
      * - checkNeg(
        s"""object Foo{ (i: Int => +i) }""",
        aggregate = """""",
      terminals = """("(" | literal | "this" | "super" | id | "_" | "{")""",
        found = ")"
      )
      * - checkNeg(
        """
          |object X {
          |  for{
          |    a <- List(1)
          |    ;
          |  } yield a
          |}""".stripMargin,
        aggregate = """""",
      terminals = """("val" | "_" | "`" | var-id | id | SimplePattern | "if")""",
        found = "} yield a"
      )
      * - checkNeg(
        """
          |object X {
          |  {
          |    val x = 1
          |    ;
          |    """.stripMargin,
        aggregate = """""",
      terminals = """(";" | "import" | "@" | local-modifier | definition | Expr | "(" | "this" | id | "_" | "}")""",
        found = ""
      )

  }
}
