package scalaparser.unit

import scalaparser.TestUtil
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
      expected = """ ")" """,
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
      expected = """((Binding ~ InfixPattern) | InfixPattern | VarId)""",
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
      expected = """ ")" """,
      found = "=> }"
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
      expected = """(("(" ~ Types.? ~ ")") | (StableId ~ ("." ~ `type`).?))""",
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
        expected = """ ")" """,
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
        expected = "((Binding ~ InfixPattern) | InfixPattern | VarId)",
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
//      * - check(
//        """object F{
//          |  this eq that.asInstanceOf[AnyRef]
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """class C{
//          |  0x01230 <= 2 && 1
//          |}
//          |
//        """.stripMargin
//      )
//      * - check(
//        """class Runtime private
//        """.stripMargin
//      )
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
//      * - check(
//        """object U{
//          |  private val _fragment = fld(Fragment)
//          |  _fld = null
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """class Array{
//          |  def length_= = 1
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object K{
//          |  def newBuilder =
//          |    new B
//          |
//          |  @inline def a = 1
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """trait Function12[-T1, +R]
//        """.stripMargin
//      )
//      * - check(
//        """@a // Don't do this at home!
//          |trait B
//        """.stripMargin
//      )
//      * - check(
//        """object T{
//          |  type B = { def F: S }
//          |}
//          |
//        """.stripMargin
//      )
//      * - check(
//        """
//          |object ScalaJSBuild{
//          |      (
//          |        1 / 2
//          |          / 3
//          |      )
//          |}
//          |
//        """.stripMargin
//      )
//      * - check(
//        """trait Writer{
//          | '\f'
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object CyclicDependencyException {
//          |    def str(info: ResolutionInfo) =
//          |      s"${info.resourceName} from: ${info.origins.mkString(", ")}"
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object OptimizerCore {
//          |  tpe match {
//          |    case NothingType | _:RecordType=> 1
//          |  }
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """class A{
//          |  1
//          |  () => 1
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """trait ReactorCanReply {
//          |  _: InternalReplyReactor =>
//          |}
//        """.stripMargin
//      )
//
//      * - check(
//        """object G{
//          |  def isBefore(pd: SubComponent) = settings.stopBefore
//          |  phaseDescriptors sliding 2 collectFirst ()
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """class SymbolLoaders {
//          |  type T = ClassPath[AbstractFile]#ClassRep
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """trait ContextErrors {
//          |    def isUnaffiliatedExpr = expanded.isInstanceOf[scala.reflect.api.Exprs#Expr[_]]
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """trait Typers{
//          |  s"nested ${ if (1) "trait" else "class" }"
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """trait ReflectSetup { this: Global =>
//          |  phase = 1
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """trait Predef {
//          |  @x
//          |  // a
//          |  type T
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """
//          object StringContext {
//
//            s"${
//              require(index >= 0 && index < str.length)
//              val ok = "[\b, \t, \n, \f, \r, \\, \", \']"
//              if (index == str.length - 1) "at terminal" else s"'\\${str(index + 1)}' not one of $ok at"
//            }"
//
//          }
//        """.stripMargin
//      )
//      * - check(
//        """trait Growable {
//          |    +=
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """package immutable {
//          |  object O
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """import java.util.concurrent.TimeUnit.{ NANOSECONDS => NANOS, MILLISECONDS ⇒ MILLIS }
//        """.stripMargin
//      )
//      * - check(
//        """class FunFinder{
//          |  val targetName = s"$name${ if (isModule) "$" else "" }"
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """class AkkaException{
//          |  for (i ← 0 until trace.length)
//          |    ()
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object Test4 {
//          |    type T = F @field
//          |    @BeanProperty val x = 1
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """package `dmacro` {
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """class A {
//          |  def fn1 = List apply 1
//          |  def fn2 = List apply[Int] 2
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """class C {
//          |  def this(x: Int) = {
//          |    this();
//          |    class D;
//          |  }
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """trait B[T] {
//          |  def f1(a: T): Unit { }
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object test {
//          |  case object Int16 extends SampleFormat1
//          |  (1) match {
//          |    case _   => 1
//          |  }
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object A {
//          |  def x {
//          |    implicit lazy val e: Int = 0
//          |  }
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object test {
//          |  for {
//          |    n <- A
//          |    a <- B
//          |    _ <- C
//          |  } yield n
//          |}
//        """.stripMargin
//      )
//      //        * - check(
//      //          """object Test {
//      //            |  def t1: M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[Inty @unchecked]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] = x
//      //            |}
//      //          """.stripMargin
//      //        )
//      * - check(
//        """abstract class Mix___eFoo___wBar_I_ extends Foo___ with Bar_I_    { ; ; f; }
//        """.stripMargin
//      )
//      * - check(
//        """package test2 {
//          |object N1M0;
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """class IP extends {
//          |  val baz = "bar";
//          |} with Foo(() => baz);
//        """.stripMargin
//      )
//      * - check(
//        """object Test extends App {
//          |  val x: C {} = 1
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """trait LensFunctions {
//          |  type T = A @> B
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object ContravariantCoyonedaUsage {
//          |  (schwartzian[Vector[String], ccord.I]
//          |      (unstructuredData)(v => ccord.k(v(i)))(ccord.fi))
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object MapTest{
//          |  forAll { a: Int ==>> Int =>
//          |  }
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object Test {
//          |  def countingDownActor = {
//          |    val ms = 1
//          |    (m: Int) =>
//          |      val x = 1
//          |      1
//          |  }
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |  type T = {
//          |    ;;;
//          |    type x = Int
//          |    ;;;
//          |    type y = Int
//          |    ;;;
//          |  }
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |  <div />
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |  <div id="hello" />
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |  <url>https://github.com/lihaoyi/scalatags</url>
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |  <url>{ ;;;1 + 1 }</url>
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object UserAgentCalculator extends Factory {
//          |    for {
//          |      userAgent <- userAgent
//          |      findResult = ieMatch.find if findResult
//          |    } yield ver
//          |}
//        """.stripMargin
//      )
//      //        * - check(
//      //          """class FunctionalBuilder{
//      //            |  a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21), a22)
//      //            |}
//      //          """.stripMargin
//      //        )
//      * - check(
//        """class HtmlPage {
//          |  <meta http-equiv="content-type" content={ 1 }/>
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object K{
//          |  <script> {{</script>
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object O{
//          |  e match { case <title>{ _* }</title> => }
//          |}
//          |
//        """.stripMargin
//      )
//      * - check(
//        """object Publish {
//          |  val x =
//          |    <inceptionYear>2009</inceptionYear>
//          |
//          |
//          |
//          |      <scm>
//          |        <url>git://github.com/akka/akka.git</url>
//          |        <connection>scm:git:git@github.com:akka/akka.git</connection>
//          |      </scm>
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object K{
//          |    <foo baz="&amp;dog"/>
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |   pomExtra :=
//          |      <url>https://github.com/lihaoyi/scalatags</url>
//          |        <licenses>
//          |          <license>
//          |            <name>MIT license</name>
//          |            <url>http://www.opensource.org/licenses/mit-license.php</url>
//          |          </license>
//          |        </licenses>
//          |        <scm>
//          |          <url>git://github.com/lihaoyi/scalatags.git</url>
//          |          <connection>scm:git://github.com/lihaoyi/scalatags.git</connection>
//          |        </scm>
//          |        <developers>
//          |          <developer>
//          |            <id>lihaoyi</id>
//          |            <name>Li Haoyi</name>
//          |            <url>https://github.com/lihaoyi</url>
//          |          </developer>
//          |        </developers>
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |  // parses as ~?>.$(1)
//          |  ~?>$ 1
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |  object Y{var z = 1}
//          |  Y z = 2
//          |  case class Z(z: Int){case class X(x: Int){ var y = 1}}
//          |  Z apply 1 X 2 y = 4
//          |
//          |  a m "txt" t = 18; println("x")
//          |}
//        """.stripMargin
//      )
//      * - check(
//        """object X{
//          |  type x = {def t: Int = 1}
//          |}
//        """.stripMargin
//      )
// What's wrong with this? I don't remember
//      * - checkNeg(
//        """
//          |object System {
//          |  def a[@b T[V @b]] = 1
//          |}
//          |
//        """.stripMargin,
//        "",
//        "asdasd"
//      )


  }
}
