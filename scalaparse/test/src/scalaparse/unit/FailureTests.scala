package scalaparse.unit

import scalaparse.TestUtil
import utest._
import TestUtil._
object FailureTests extends TestSuite{
  val tests = Tests {

    * - checkNeg("package package", """(Id | "case" | "object")""", "package")

    * - checkNeg(
      """package torimatomeru
        |import a
        |import import
      """.stripMargin,
      expected = """("this" | "super" | Id)""",
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
      expected = """(Id | "<-" | "←" | "=")""",
      found = "} yield x"
    )
    * - checkNeg(
      """object O{
        |  type T = (A B)
        |}
      """.stripMargin,
      expected = """("(" | literal | "this" | "super" | Id | "_" | "{")""",
      found = ")"
    )
    * - checkNeg(
      """object O{
        | def from(n: Long, c: Int = 0): Int =
        |  if (n == 1) c + 1 else
        |}
      """.stripMargin,
      expected = """Expr""",
      found = ""
    )
    * - checkNeg(
      "import scala.util.{Failure, Success + 1}",
      expected = """("=>" | "⇒" | "," | "}")""",
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
      expected = """("(" | ":" | "=" | "{" | ";" | "}")""",
      found = "](input: S"
    )
    * - checkNeg(
      """
        |object SyntaxTest{
        |  a(
        |  throw 1
        |}
      """.stripMargin,
      expected = """("." | "[" | "(" | "{" | "_" | Id | "=>" | "⇒" | "=" | "match" | ":" | "," | ")")""",
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
      expected = """("=>" | "⇒" | ":" | "." | "[" | "(" | "{" | "_" | Id | "=" | "match" | ";" | "}")""",
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
      expected = """("this" | Id)""",
      found = "."
    )
    * - checkNeg(
      """
        |object Moo{
        | filename.asInstanceOf 10
        |}
      """.stripMargin,
      expected = """("." | "[" | "(" | "{" | "_" | Id | "=>" | "⇒" | "=" | "match" | ":" | ";" | "}")""",
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
      expected = """("extends" | "<:" | "{" | ";" | end-of-input)""",
      found = "o w{"
    )
    * - checkNeg(
      """
        |object O{
        | private[this] applyMacroFull = 1
        |}
      """.stripMargin,
      expected = """(local-modifier | "private" | "protected" | "override" | definition)""",
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
      expected = """("=>" | "⇒" | "(" | literal | "this" | "super" | Id | "_" | "{")""",
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
      expected = """Id""",
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
      expected = """("." | "," | ";" | end-of-input)""",
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
      expected = """(XmlPattern | "_" | literal | "(" | "this" | "super" | Id | !AlphabetKeywords | SymbolicKeywords)""",
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
      expected = """("." | "[" | "(" | "{" | "_" | Id | "=>" | "⇒" | "=" | "match" | ":" | ";" | "}")""",
      found = ")"
    )
    * - checkNeg(
      """
        |object O{
        |   A(A(A(A(A(A(A(A(A(A(A(A(A(A(A(A()))))))))))))))
        |}
      """.stripMargin,
      expected = """("." | "[" | "(" | "{" | "_" | Id | "=>" | "⇒" | "=" | "match" | ":" | "," | ")")""",
      found = "}"
    )
    * - checkNeg(
      """
        |object L{
        |  a.b =
        |}
      """.stripMargin,
      expected = "Expr",
      found = "}"
    )
    * - checkNeg(
      """
        |object L{
        |  a b c
        |  d = 1
        |
      """.stripMargin,
      expected = """("." | "[" | "=>" | "⇒" | "=" | "match" | ":" | ";" | "}")""",
      found = ""
    )
    * - checkNeg(
      """/*
        |
        |package scala.scalajs.cli
        |
      """.stripMargin,
      expected = """ "*/" """,
      found = ""
    )
    * - checkNeg(
      """/*/*
        |*/
        |package scala.scalajs.cli
        |
      """.stripMargin,
      expected = """ "*/" """,
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
      expected = """("yield" | Expr)""",
      found = "}"
    )
    * - checkNeg(
      """
        |object O{
        |  val jarFile = catch { case _: F => G }
        |}
      """.stripMargin,
      expected = "Expr",
      found = "catch {"
    )
    * - checkNeg(
      """
        |object F{
        |  func{ case _: F = fail }
        |}
      """.stripMargin,
      expected = """("." | "[" | "#" | "@" | "with" | "{" | "|" | "if" | "=>" | "⇒")""",
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
      expected = """(XmlPattern | "_" | literal | "(" | "this" | "super" | Id | !AlphabetKeywords)""",
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
      expected = """(":" | "@" | "." | "[" | "(" | Id | "|" | "," | ")")""",
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
      expected = """("=>" | "⇒" | "(" | literal | "this" | "super" | Id | "_" | "{")""",
      found = "val c"
    )
    * - checkNeg(
      """
        |object LOLS{
        |  def run(def apply() {}) {}
        |}
      """.stripMargin,
      expected = """("implicit" | "@" | Id | ")")""",
      found = "def apply"
    )
    * - checkNeg(
      """
        |object O{
        |  a =:= .c
        |}
      """.stripMargin,
      expected = """("[" | [\\-+!~] | XmlExpr | "new" | "{" | literal | "this" | "super" | Id | "_" | "(" | "=>" | "⇒" | "=" | "match" | ":" | ";" | "}")""",
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
      expected = """("_" | "=>" | "⇒" | "(" | literal | "this" | "super" | Id | "{" | "@")""",
      found = ")\n}"
    )
    * - checkNeg(
      """
        |object K{
        |  a[)
        |}
      """.stripMargin,
      expected = """("=>" | "⇒" | "(" | literal | "this" | "super" | Id | "_" | "{" | "," | "]")""",
      found = ")"
    )
    * - checkNeg(
      """
        |object K{
        |  a[b)
        |}
      """.stripMargin,
      expected = """("\"\"\"" | "\"" | "." | "[" | "#" | "@" | "with" | "{" | "*" | Id | "=>" | "⇒" | "forSome" | ">:" | "<:" | "," | "]")""",
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
      expected = """ "case" """,
      found = "stats :+ e"
    )

    * - checkNeg(
      """
        |object K
        |  val trueA = 1
        |}
      """.stripMargin,
      expected = """("extends" | "<:" | "{" | ";" | "package" | "import" | "@" | local-modifier | "private" | "protected" | "override" | "trait" | "case" | "class" | "object" | end-of-input)""",
      found = "val trueA"
    )
    * - checkNeg(
      """
        |object K{
        |  val null null cow = 1
        |}
      """.stripMargin,
      expected = """(Id | "," | ":" | "=" | ";" | "}")""",
      found = "null cow"
    )
    * - checkNeg(
      """
        |object K{
        |  val omg_+_+ = 1
        |}
      """.stripMargin,
      expected = """("@" | "\"\"\"" | "\"" | "." | "[" | "(" | Id | "," | ":" | "=" | ";" | "}")""",
      found = "_+ = 1"
    )
    * - checkNeg(
      """
        |object K{
        |  val + = 1
        |  var = 2
        |}
      """.stripMargin,
      expected = """(Id | "_" | XmlPattern | literal | "(" | "this" | "super" | !AlphabetKeywords | SymbolicKeywords)""",
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
      expected = """ (XmlPattern | "_" | literal | "(" | "this" | "super" | Id | !AlphabetKeywords | SymbolicKeywords)""",
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
      expected = """("@" | "." | "[" | "(" | Id | "|" | "if" | "=>" | "⇒")""",
      found = "case"
    )
    * - checkNeg(
      """trait Basic{
        |  a!.b
        |}
      """.stripMargin,
      expected = """("[" | [\\-+!~] | XmlExpr | "new" | "{" | literal | "this" | "super" | Id | "_" | "(" | "=>" | "⇒" | "=" | "match" | ":" | ";" | "}")""",
      found = ".b"
    )
    * - checkNeg(
      """
        |class Parser {
        |  {( => }
        |}
        |
      """.stripMargin,
      expected = """("(" | "this" | Id | "_" | Expr | "," | ")")""",
      found = "=> }"
    )
    * - checkNeg(
      """
        |class Parser([
        |
      """.stripMargin,
      expected = """("implicit" | "@" | local-modifier | "private" | "protected" | "override" | "val" | "var" | Id | "," | ")")""",
      found = "["
    )
    * - checkNeg(
      """
        |class Parser{
        | @mog
        |}
      """.stripMargin,
      expected = """("." | "[" | "#" | "(" | "@" | local-modifier | "private" | "protected" | "override" | definition)""",
      found = "}"
    )
    * - checkNeg(
      """class C
        |package omg
        |;
      """.stripMargin,
      expected = """("." | "{")""",
      found = ";"
    )

    * - checkNeg(
      """object B {
        |  { a: L = }
        |}
      """.stripMargin,
      expected = """("." | "[" | "#" | "@" | "with" | "{" | "*" | Id | "=>" | "⇒" | "(" | "this" | "_" | "import" | local-modifier | definition | Expr | ";" | "}")""",
      found = "= }"
    )
      * - checkNeg(
        """object GenJSCode{
          |  a.b.()
          |}
          |
        """.stripMargin,
        expected = """("this" | Id)""",
        found = "()"
      )
    * - checkNeg(
      """object GenJSCode{
        |  this.this.()
        |}
        |
      """.stripMargin,
      expected = """(!`super` | "this" ~ !LetterDigitDollarUnderscore | Id)""",
      found = "this"
    )
    * - checkNeg(
      """object GenJSCode{
        |  null: b.()
        |}
        |
      """.stripMargin,
      expected = """("this" | Id)""",
      found = "()"
    )
      * - checkNeg(
        """object K{
          |  private O
          |}
        """.stripMargin,
        expected = """("[" | local-modifier | "private" | "protected" | "override" | definition)""",
        found = "O\n"
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
        expected = """(":" | "@" | "." | "[" | "(" | Id | "|" | "," | ")")""",
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
      expected = """(Id | "<-" | "←" | "=")""",
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
      expected = """("." | "[" | "=>" | "⇒" | "=" | "match" | ":" | "if" | ";" | "val" | "_" | "`" | character-predicate | Id | XmlPattern | literal | "(" | "this" | "super" | "}")""",
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
        expected = "Expr",
        found = "}"
      )
      * - checkNeg(
        """
          |object ScopedVar {
          |  def withScopedVars(ass: Seq[_, ]) = 1
          |}
          |
        """.stripMargin,
        expected = """("[" | "#" | "@" | "with" | "{" | "*" | Id | "=>" | "⇒" | "forSome" | ">:" | "<:" | literal | "]")""",
        found = ", ]"
      )
      * - checkNeg(
        """
          |abstract class JSASTTest extends DirectTest {
          |  def show: this.type = )
          |}
          |
        """.stripMargin,
        expected = """("macro" | Expr)""",
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
        expected = """("." | "[" | "=>" | "⇒" | "=" | "match" | ":" | ";" | "(" | "this" | Id | "_" | "import" | "@" | local-modifier | definition | Expr | "}")""",
        found = "case for"
      )
      * - checkNeg(
        """object Utils {
          |  "\q"
          |}
          |
        """.stripMargin,
        expected = """([btnfr'\\\\\"]] | [0-9] | "u")""",
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
        expected = """(chars-while(1) | "\\" | ![\n\"] | "\"")""",
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
        expected = """(chars-while(1) | "\"" | [\\\\$\n] | "\"\"\"")""",
        found = ""
      )
      * - checkNeg(
        """
          |object X{
          |  ''
          |}
          |
        """.stripMargin,
        expected = """("\\" | character-predicate | chars-while(1) | "/" | AlphabetKeywords | SymbolicKeywords)""",
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
        expected = """([0-9] | Id)""",
        found = ")"
      )
      * - checkNeg(
        s"""
           |object X{
           |  1..toString
           |}
        """.stripMargin,
        expected = """([0-9] | Id)""",
        found = ".toString"
      )
      * - checkNeg(
        s"""
           |object X{
           |  val x, = 1
           |}
        """.stripMargin,
        expected = """(Id | "_" | XmlPattern | literal | "(" | "this" | "super" | !AlphabetKeywords | SymbolicKeywords)""",
        found = "= 1"
      )
    * - checkNeg(
      s"""
         |object X{
         |  val (x,) = 1
         |}
        """.stripMargin,
      expected = """(chars-while(1) | [_] | ":" | "@" | "\"\"\"" | "\"" | "." | "[" | "(" | Id | "|" | XmlPattern | literal | ")")""",
      found = ",)"
    )
    * - checkNeg(
      s"""
         |object X{ val (_:) = 1 }
        """.stripMargin,
      expected = """("(" | literal | "this" | "super" | Id | "_" | "{")""",
      found = ") = 1"
    )
    * - checkNeg(
      s"""
         |import x.{y=>}
        """.stripMargin,
      expected = """(Id | "_")""",
      found = "}"
    )
    * - checkNeg(
      s"""
         |import x.y,
        """.stripMargin,
      expected = """("this" | "super" | Id)""",
      found = ""
    )
    * - checkNeg(
      s"""
         |object X{type T = A with}
        """.stripMargin,
      expected = """("(" | literal | "this" | "super" | Id | "_")""",
      found = "}"
    )
    * - checkNeg(
      s"""
         |object X{def f(x: Int, ) = 1}
        """.stripMargin,
      expected = """("\"\"\"" | "\"" | "." | "[" | "#" | "@" | "with" | "{" | "*" | Id | "=>" | "⇒" | "forSome" | ">:" | "<:" | "=" | ")")""",
      found = ", )"
    )
    * - checkNeg(
      s"""
         |object X{val x = (1, 2,)}
        """.stripMargin,
      expected = """("." | [Ee] | [fFdD] | "L" | "l" | "[" | "(" | "{" | "_" | Id | "=>" | "⇒" | "=" | "match" | ":" | Expr | ")")""",
      found = ",)"
    )
    * - checkNeg(
      s"""
         |object X{f[A,]}
        """.stripMargin,
      expected = """("\"\"\"" | "\"" | "." | "[" | "#" | "@" | "with" | "{" | "*" | Id | "=>" | "⇒" | "forSome" | ">:" | "<:" | literal | "]")""",
      found = ",]"
    )
    * - checkNeg(
      s"""
         |object X{def f[T <% A <%] = 1}
        """.stripMargin,
      expected = """("=>" | "⇒" | "(" | literal | "this" | "super" | Id | "_" | "{")""",
      found = "]"
    )
    * - checkNeg(
      s"""
         |object X{def f[T, B,] = 1}
        """.stripMargin,
      expected = """("[" | ">:" | "<:" | "<%" | ":" | "]")""" ,
      found = ",]"
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
      expected = """Expr""",
      found = ") = 1"
    )
    * - checkNeg(
      s"""
         |object X{type T = Int#}
        """.stripMargin,
      expected = """Id""",
      found = "}"
    )
    * - checkNeg(
      s"""
         |object X{type T = }
        """.stripMargin,
      expected = """("=>" | "⇒" | "(" | literal | "this" | "super" | Id | "_" | "{")""",
      found = "}\n"
    )
    * - checkNeg(
      s"""
         |object X{type T[,] = A }
        """.stripMargin,
      expected = """("@" | [+\\-] | Id | "_")""" ,
      found = ",]"
    )
    * - checkNeg(
      s"""
         |object X{type T <: }
        """.stripMargin,
      expected = """("=>" | "⇒" | "(" | literal | "this" | "super" | Id | "_" | "{")""",
      found = "}\n"
    )
      * - checkNeg(
        """
          |object System {
          |  def a[@b T @f [@b V]] = 1
          |}
          |
        """.stripMargin,
        expected = """("[" | ">:" | "<:" | "<%" | ":" | "," | "]")""",
        found = "@f"
      )
      * - checkNeg(
        """
          |@func(} object System {
          |
          |}
          |
        """.stripMargin,
        expected = """(Expr | "," | ")")""",
        found = "}"
      )
      * - checkNeg(
        """
          |object System {
          |  def f[[a] = 1
          |}
          |
        """.stripMargin,
        expected = """("@" | Id | "_")""",
        found = "["
      )

      * - checkNeg(
        s"""
          |object System {
          |  $tq """".stripMargin,
        expected = """("\"" | chars-while(1) | [\\\\$\n] | "\"\"\"")""",
        found = ""
      )
      * - checkNeg(
        """
          |object System {
          |  e match { case <xml:unparsed><</xml:unparsed> => }
          |}
          |
        """.stripMargin,
        expected = """("&" | !"<" | "{{" | "{" | "</")""",
        found = "<</xml:unp"
      )

      * - checkNeg(
        s"""object Foo{
           |  for(i <- Nil if x: Int => bar) 1
           |}
         """.stripMargin,
        expected = """("\"\"\"" | "\"" | "." | "[" | "(" | "{" | "_" | Id | ";" | "val" | "`" | !AlphabetKeywords | SymbolicKeywords | XmlPattern | literal | "this" | "super" | "if" | ")")""",
        found = ": Int"
      )
      * - checkNeg(
        s"""object Foo{; x: Int => x}""",
        expected = """("." | "[" | "#" | "@" | "with" | "{" | "*" | Id | ";" | "}")""",
        found = "=> x"
      )
      * - checkNeg(
        s"""object Foo{ (i: Int => +i) }""",
        expected = """("(" | literal | "this" | "super" | Id | "_" | "{")""",
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
        expected = """("val" | "_" | "`" | character-predicate | Id | XmlPattern | literal | "(" | "this" | "super" | "if")""",
        found = "} yield a"
      )
      * - checkNeg(
        """
          |object X {
          |  {
          |    val x = 1
          |    ;
          |    """.stripMargin,
        expected = """(";" | "import" | "@" | local-modifier | definition | Expr | "(" | "this" | Id | "_" | "}")""",
        found = ""
      )

  }
}
