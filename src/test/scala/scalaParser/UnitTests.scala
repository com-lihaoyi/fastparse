package scalaParser

import org.parboiled2.ParseError
import utest._

import scala.util.{Failure, Success}

object UnitTests extends TestSuite{
  def checkNeg[T](input: String) = {
    println("Checking...")
    new Scala(input).CompilationUnit.run() match{
      case Failure(f: ParseError) => () // yay
      case Success(parsed) => assert(parsed.length != input.length)
    }
  }
  def check[T](input: String) = {
    println("Checking...")
    new Scala(input).CompilationUnit.run() match{
      case Failure(f: ParseError) =>
        println(f.position)
        println(f.formatExpectedAsString)
        println(f.formatTraces)
        throw new Exception(f.position + "\t" + f.formatTraces)
      case Success(parsed) =>
        if(parsed != input)

          throw new Exception(
            "Parsing Failed at " + parsed.length + "\n" + input.drop(parsed.length).take(50)
          )
    }
  }
  println("running")
  def tests = TestSuite{
    'pos {
      * - check(
        "package torimatomeru"

      )
      * - check(
        """package torimatomeru
          |
          |package lols
        """.stripMargin
      )
      * - check(
        """package torimatomeru
          |import a
          |import b
        """.stripMargin
      )
      * - check(
        """
          |package torimatomeru
          |
          |import org.parboiled2.ParseError
          |import utest._
          |import utest.framework.Test
          |import utest.util.Tree
          |
          |import scala.util.{Failure, Success}
          |
          |object SyntaxTest extends TestSuite
        """.stripMargin
      )
      * - check(
        """
          |object SyntaxTest extends TestSuite{
          |  def check[T](input: String) = {
          |
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """
          |object SyntaxTest{
          |  a()
          |  throw 1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object SyntaxTest extends TestSuite{
          |  {
          |        println
          |        throw 1
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """package scalatex
          |
          |
          |import org.parboiled2._
          |import torimatomeru.ScalaSyntax
          |
          |import scalatex.stages.{Trim, Parser, Ast}
          |import scalatex.stages.Ast.Block.{IfElse, For, Text}
          |import Ast.Chain.Args
          |
          |object ParserTests extends utest.TestSuite{
          |  import Ast._
          |  import utest._
          |  def check[T](input: String, parse: Parser => scala.util.Try[T], expected: T) = {
          |    val parsed = parse(new Parser(input)).get
          |    assert(parsed == expected)
          |  }
          |  def tests = TestSuite{}
          |}
        """.stripMargin
      )
      * - check(
        """
          |object Moo{
          |  a
          |  .b
          |
          |  c
          |}
        """.stripMargin
      )
      * - check(
        """
          |object Moo{
          | filename
          |        .asInstanceOf[Literal]
          |10
          |}
        """.stripMargin
      )
      * - check(
        """
          |object Cow{
          |  ().mkString
          |
          |  1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object O{
          | private[this] val applyMacroFull = 1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object O{
          | private[this] def applyMacroFull(c: Context)
          |                      (expr: c.Expr[String],
          |                       runtimeErrors: Boolean,
          |                       debug: Boolean)
          |                      : c.Expr[Frag] = {
          |                      }
          |}
        """.stripMargin
      )
      * - check(
        """
          |object O{
          |  class DebugFailure extends Exception
          |
          |  1
          |}
        """.stripMargin
      )
      * - check(
        """
          |package torimatomeru
          |
          |package syntax
          |
          |import org.parboiled2._
          |
        """.stripMargin
      )
      * - check(
        """
          |object Foo{
          |  0 match {
          |    case A | B => 0
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """
        |object Compiler{
        |
        |  def apply = {
        |    def rec = t match {
        |      case 0 => 0
        |    }
        |
        |    rec(tree)
        |  }
        |}
        |
      """.
          stripMargin
      )
      * - check(
        """
          |object O {
          |    A(A(A(A(A(A(A(A())))))))
          |}
          |
        """.stripMargin
      )
      * - check(
        """
          |object O{
          |   A(A(A(A(A(A(A(A(A(A(A(A(A(A(A(A())))))))))))))))
          |}
        """.stripMargin
      )
      * - check(
        """
          |object L{
          |  a.b = c
          |  a().b = c
          |}
        """.stripMargin
      )
      * - check(
        """
          |object L{
          |  a b c
          |  d = 1
          |}
        """.stripMargin
      )

      * - check(
        """/*                     __                                               *\
          |**     ________ ___   / /  ___      __ ____  Scala.js CLI               **
          |**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
          |**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
          |** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
          |**                          |/____/                                     **
          |\*                                                                      */
          |
          |package scala.scalajs.cli
          |
        """.stripMargin
      )
      * - check(
        """
          |object O{
          |  for {
          |      a  <- b
          |      c <- d
          |  } {
          |    1
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """
          |object O{
          |  val jarFile =
          |      try { 1 }
          |      catch { case _: F => G }
          |}
        """.stripMargin
      )
      * - check(
        """
          |object F{
          |  func{ case _: F => fail }
          |}
        """.stripMargin
      )
      * - check(
        """
          |object Foo{
          |    val a = d // g
          |    val b = e // h
          |    val c = f
          |}
        """.stripMargin
      )
      * - check(
        """
          |object L{
          |  x match{
          |    case y.Y(z) => z
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """object K{
          |  val a: B {
          |    val c: D
          |  }
          |
          |  1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object LOLS{
          |    def run() {}
          |
          |    def apply() {}
          |}
        """.stripMargin
      )
      * - check(
        """
          |object O{
          |  a =:= b.c
          |}
        """.stripMargin
      )
      * - check(
        """
          |object K{
          |  a(
          |    1: _*
          |  )
          |}
        """.stripMargin
      )
      * - check(
        """
          |object P{
          |      tree match {
          |        case stats :+ expr  => 1
          |      }
          |}
        """.stripMargin
      )
      * - check(
        """
          |object K{
          |  val trueA = 1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object K{
          |  val nullo :: cow = 1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object K{
          |  val omg_+ = 1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object K{
          |  val + = 1
          |  var * = 2
          |}
        """.stripMargin
      )
      * - check(
        """
          |object O{
          |  c match {
          |    case b_  => 1
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """
          |trait Basic {
          |  b match {
          |    case C => true; case _ => false
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """trait Basic {
          |  !a.b
          |}
        """.stripMargin
      )
      * - check(
        """
          |class Parser {
          |  {() => }
          |}
          |
        """.stripMargin
      )
      * - check(
        """
          |
          |
          |
          |package omg
          |;
          |
          |;
          |
          |;
          |class Parser
          |;
          |
          |;
          |
          |;
        """.stripMargin
      )
      * - check(
        """
          |
          |object GenJSCode {
          |  code: @switch
          |}
        """.stripMargin
      )
      * - check(
        """object B {
          |  { a: L => }
          |}
        """.stripMargin
      )
      * - check(
        """object O{
          |  {
          |    val index = 0
          |    i: Int => 10
          |    0
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """object GenJSCode{
          |  val g: G.this.g.type
          |}
          |
        """.stripMargin
      )
      * - check(
        """object K{
          |  class RTTypeTest
          |  private object O
          |}
        """.stripMargin
      )
      * - check(
        """object O{
          |  if (eqeq &&
          |
          |    false)  1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object O{
          |  for(
          |    x <- Nil map
          |
          |  (x => x)
          |  ) yield x
          |}
        """.stripMargin
      )
      * - check(
        """
          |object O{
          |  for{
          |    x <- Nil
          |    if
          |
          |    1 == 2
          |  } yield x
          |}
        """.stripMargin
      )
      * - check(
        """
          |object ScopedVar {
          |  def withScopedVars(ass: Seq[_]) = 1
          |}
          |
        """.stripMargin
      )
      * - check(
        """
          |abstract class JSASTTest extends DirectTest {
          |  def show: this.type = ()
          |}
          |
        """.stripMargin
      )
      * - check(
        """object Traversers {
          |  {
          |        1
          |        cases foreach nil
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """object Utils {
          |  "\\"
          |}
          |
        """.stripMargin
      )
      * - check(
        """object F{
          |  this eq that.asInstanceOf[AnyRef]
          |}
        """.stripMargin
      )
      * - check(
        """class C{
          |  0x00 <= 2 && 1
          |}
          |
        """.stripMargin
      )
      * - check(
        """class Runtime private
        """.stripMargin
      )
      * - check(
        """
          |object System {
          |  def a[@b T[@b V]] = 1
          |}
          |
        """.stripMargin
      )
      * - check(
        """object U{
          |  private val _fragment = fld(Fragment)
          |  _fld = null
          |}
        """.stripMargin
      )
      * - check(
        """class Array{
          |  def length_= = 1
          |}
        """.stripMargin
      )
      * - check(
        """object K{
          |  def newBuilder =
          |    new B
          |
          |  @inline def a = 1
          |}
        """.stripMargin
      )
      * - check(
        """trait Function12[-T1, +R]
        """.stripMargin
      )
      * - check(
        """@a // Don't do this at home!
          |trait B
        """.stripMargin
      )
      * - check(
        """object T{
          |  type B = { def F: S }
          |}
          |
        """.stripMargin
      )
      * - check(
        """
          |object ScalaJSBuild{
          |      (
          |        1 / 2
          |          / 3
          |      )
          |}
          |
        """.stripMargin
      )
      * - check(
        """trait Writer{
          | '\f'
          |}
        """.stripMargin
      )
      * - check(
        """object CyclicDependencyException {
          |    def str(info: ResolutionInfo) =
          |      s"${info.resourceName} from: ${info.origins.mkString(", ")}"
          |}
        """.stripMargin
      )
      * - check(
        """object OptimizerCore {
          |  tpe match {
          |    case NothingType | _:RecordType=> 1
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """class A{
          |  1
          |  () => 1
          |}
        """.stripMargin
      )
      * - check(
        """trait ReactorCanReply {
          |  _: InternalReplyReactor =>
          |}
        """.stripMargin
      )

      * - check(
        """object G{
          |  def isBefore(pd: SubComponent) = settings.stopBefore
          |  phaseDescriptors sliding 2 collectFirst ()
          |}
        """.stripMargin
      )
      * - check(
        """class SymbolLoaders {
          |  type T = ClassPath[AbstractFile]#ClassRep
          |}
        """.stripMargin
      )
      * - check(
        """trait ContextErrors {
          |    def isUnaffiliatedExpr = expanded.isInstanceOf[scala.reflect.api.Exprs#Expr[_]]
          |}
        """.stripMargin
      )
      * - check(
        """trait Typers{
          |  s"nested ${ if (1) "trait" else "class" }"
          |}
        """.stripMargin
      )
      * - check(
        """trait ReflectSetup { this: Global =>
          |  phase = 1
          |}
        """.stripMargin
      )
      * - check(
        """trait Predef {
          |  @x
          |  // a
          |  type T
          |}
        """.stripMargin
      )
      * - check(
        """
          object StringContext {

            s"${
              require(index >= 0 && index < str.length)
              val ok = "[\b, \t, \n, \f, \r, \\, \", \']"
              if (index == str.length - 1) "at terminal" else s"'\\${str(index + 1)}' not one of $ok at"
            }"

          }
        """.stripMargin
      )
      * - check(
        """trait Growable {
          |    +=
          |}
        """.stripMargin
      )
      * - check(
        """package immutable {
          |  object O
          |}
        """.stripMargin
      )
      * - check(
        """import java.util.concurrent.TimeUnit.{ NANOSECONDS => NANOS, MILLISECONDS ⇒ MILLIS }
        """.stripMargin
      )
      * - check(
        """class FunFinder{
          |  val targetName = s"$name${ if (isModule) "$" else "" }"
          |}
        """.stripMargin
      )
      * - check(
        """class AkkaException{
          |  for (i ← 0 until trace.length)
          |    ()
          |}
        """.stripMargin
      )
      * - check(
        """object Test4 {
          |    type T = F @field
          |    @BeanProperty val x = 1
          |}
        """.stripMargin
      )
      * - check(
        """package `dmacro` {
          |}
        """.stripMargin
      )
      * - check(
        """class A {
          |  def fn1 = List apply 1
          |  def fn2 = List apply[Int] 2
          |}
        """.stripMargin
      )
      * - check(
        """class C {
          |  def this(x: Int) = {
          |    this();
          |    class D;
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """trait B[T] {
          |  def f1(a: T): Unit { }
          |}
        """.stripMargin
      )
      * - check(
        """object test {
          |  case object Int16 extends SampleFormat1
          |  (1) match {
          |    case _   => 1
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """object A {
          |  def x {
          |    implicit lazy val e: Int = 0
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """object test {
          |  for {
          |    n <- A
          |    a <- B
          |    _ <- C
          |  } yield n
          |}
        """.stripMargin
      )
//        * - check(
//          """object Test {
//            |  def t1: M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[Inty @unchecked]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] = x
//            |}
//          """.stripMargin
//        )
      * - check(
        """abstract class Mix___eFoo___wBar_I_ extends Foo___ with Bar_I_    { ; ; f; }
        """.stripMargin
      )
      * - check(
        """package test2 {
          |object N1M0;
          |}
        """.stripMargin
      )
      * - check(
        """class IP extends {
          |  val baz = "bar";
          |} with Foo(() => baz);
        """.stripMargin
      )
      * - check(
        """object Test extends App {
          |  val x: C {} = 1
          |}
        """.stripMargin
      )
      * - check(
        """trait LensFunctions {
          |  type T = A @> B
          |}
        """.stripMargin
      )
      * - check(
        """object ContravariantCoyonedaUsage {
          |  (schwartzian[Vector[String], ccord.I]
          |      (unstructuredData)(v => ccord.k(v(i)))(ccord.fi))
          |}
        """.stripMargin
      )
      * - check(
        """object MapTest{
          |  forAll { a: Int ==>> Int =>
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """object Test {
          |  def countingDownActor = {
          |    val ms = 1
          |    (m: Int) =>
          |      val x = 1
          |      1
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """object X{
          |  type T = {
          |    ;;;
          |    type x = Int
          |    ;;;
          |    type y = Int
          |    ;;;
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """object X{
          |  <div />
          |}
        """.stripMargin
      )
      * - check(
        """object X{
          |  <div id="hello" />
          |}
        """.stripMargin
      )
      * - check(
        """object X{
          |  <url>https://github.com/lihaoyi/scalatags</url>
          |}
        """.stripMargin
      )
      * - check(
        """object X{
          |  <url>{ ;;;1 + 1 }</url>
          |}
        """.stripMargin
      )
      * - check(
        """object UserAgentCalculator extends Factory {
          |    for {
          |      userAgent <- userAgent
          |      findResult = ieMatch.find if findResult
          |    } yield ver
          |}
        """.stripMargin
      )
//        * - check(
//          """class FunctionalBuilder{
//            |  a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21), a22)
//            |}
//          """.stripMargin
//        )
      * - check(
        """class HtmlPage {
          |  <meta http-equiv="content-type" content={ 1 }/>
          |}
        """.stripMargin
      )
      * - check(
        """object K{
          |  <script> {{</script>
          |}
        """.stripMargin
      )
      * - check(
        """object O{
          |  e match { case <title>{ _* }</title> => }
          |}
          |
        """.stripMargin
      )
      * - check(
        """object Publish {
          |  val x =
          |    <inceptionYear>2009</inceptionYear>
          |
          |
          |
          |      <scm>
          |        <url>git://github.com/akka/akka.git</url>
          |        <connection>scm:git:git@github.com:akka/akka.git</connection>
          |      </scm>
          |}
        """.stripMargin
      )
      * - check(
        """object K{
          |    <foo baz="&amp;dog"/>
          |}
        """.stripMargin
      )
      * - check(
        """object X{
          |   pomExtra :=
          |      <url>https://github.com/lihaoyi/scalatags</url>
          |        <licenses>
          |          <license>
          |            <name>MIT license</name>
          |            <url>http://www.opensource.org/licenses/mit-license.php</url>
          |          </license>
          |        </licenses>
          |        <scm>
          |          <url>git://github.com/lihaoyi/scalatags.git</url>
          |          <connection>scm:git://github.com/lihaoyi/scalatags.git</connection>
          |        </scm>
          |        <developers>
          |          <developer>
          |            <id>lihaoyi</id>
          |            <name>Li Haoyi</name>
          |            <url>https://github.com/lihaoyi</url>
          |          </developer>
          |        </developers>
          |}
        """.stripMargin
      )

    }
    'neg{
      * - checkNeg(
        """
          |object O{
          |  for{
          |    x <- Nil map
          |
          |  (x => x)
          |  } yield x
          |}
        """.stripMargin
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
        """.stripMargin
      )
      * - checkNeg(
        """object O{
          |  for{
          |    x <- Nil
          |    _ = 1 ==
          |
          |    2
          |  } yield x
          |}
        """.stripMargin
      )
      * - checkNeg(
        """
          |object System {
          |  def a[@b T[V @b]] = 1
          |}
          |
        """.stripMargin
      )

    }

  }
}