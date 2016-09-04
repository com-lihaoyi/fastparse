package scalaparse.unit

import scalaparse.{TestUtil, Scala}
import fastparse._
import utest._
import TestUtil._
object SuccessTests extends TestSuite{

  println("running")
  def tests = TestSuite{

    * - check("package torimatomeru")

    * - check("import com.typesafe")
    * - check(
      """package torimatomeru
        |
        |package lols
      """.stripMargin
    )
    * - check(
      """
        |trait ColumnarTableCompanion {
        |    class IndexedSource()
        |
        |    (1)
        |}
        |
      """.stripMargin
    )
    * - check(
      """package torimatomeru
        |import a
        |import b
      """.stripMargin
    )
    * - check(
      """object Foo{
        |  val a = (null): Int => Int
        |}
      """.stripMargin
    )
    * - check(
      """object X{
        |  for {
        |    A <- x()
        |    (_, node) <- signature
        |  } ()
        |}
      """.stripMargin
    )
    * - check(
      """
        |trait TaskExtra {
        |  { x => x: A }
        |}
        |
      """.stripMargin
    )
    * - check(
      """class ScProjectionType (val x: Boolean /* todo*/)""".stripMargin
    )
    * - check(
      """
        |class Baz {
        |    ba /*caret*/r
        |}
        |""".stripMargin
    )
    * - check(
      """class GroupManager @Singleton @Inject() ()""".stripMargin
    )
    * - check(
      """object O{
        |  val service = Service.mk { _: Request => Future.value(res) }
        |  val service2 = Service.mk { _ => Future.value(res) }
        |}
      """.stripMargin
    )
    * - check(
      """object Foo{
        |  q"\$result.putExtra(\${toName(c)(value)}, \$value)"
        |}
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
      """class JSCodePhase{
        |  def genRTCall(args: Tree*)
        |}
      """.stripMargin
    )
    * - check(
      """object X{
        |  {
        |    ((newEntries)): Int => 1
        |  }
        |}
        |
      """.stripMargin
    )
    * - check(
      """object X{ ((newEntries)): Int => 1
        |
        |}
        |
      """.stripMargin
    )
    * - check(
      """class NodexSpec extends Spec {
        |  val x = ! <a/>.isSpaceNode
        |}
      """.stripMargin
    )
    * - check(
      """object O {
        |  def x: Q.type#/* line: 2 */Inner
        |}
      """.stripMargin
    )
    * - check(
      """object O {
        |  def x: Q.type#/* line: 2 */Inner
        |}
      """.stripMargin
    )
    * - check(
      """object O {
        |  val x = (x +/* */ 1)
        |}
      """.stripMargin
    )
    * - check(
      """object O {
        |  val x = x +// {
        |}
      """.stripMargin
    )
    * - check(
      """object O {
        |  val x:// {{{
        |    Int
        |}
      """.stripMargin
    )
    * - check(
      """object O {
        |  val x: A +// {{{
        |    Int
        |}
      """.stripMargin
    )
    * - check(
      """class OptionLikeMatcher{
        |  type A = _ \/ B
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
        |class TracingFilterBenchmark {
        |  ZipkinTracer(
        |    a,
        |    if (on) 1.0f else 0.0f
        |  )
        |}
        |
      """.stripMargin
    )
    * - check(
      """
        |class A(
        |       @volatile var getCollectionCount: Long,
        |       @volatile var getCollectionTime: Long
        |       )
        |
      """.stripMargin
    )
    * - check(
      """class NodexSpec {
        |  (! <a/>.isSpaceNode)
        |  ! <a/>.isSpaceNode
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
      """
        |object ParserTests {
        |  def f(x: Try[T])
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
      """object O{
        |  { case _: Int | () => 1 }
        |}
      """.stripMargin
    )
    * - check(
      """
        |object Test {
        |  ucheck(id_TSa_T [Unit   , Array[Unit   ]](uarray));
        |}
        |
      """.stripMargin
    )
    * - check(
      """class SymbolicXMLBuilder {
        |  (_scala_xml_PrefixedAttribute  , 1)
        |}
      """.stripMargin
    )
    * - check(
      """
        |object Implicits {
        |  x: Sequencer[D, (T1)]
        |}
        |
      """.stripMargin
    )
    * - check(
      """
        |object A extends B with (C)
        |
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
      """
        |trait BuiltinCommands {
        |  val x = s => {}
        |
        |  @tailrec private[this] def doLoadFailed
        |}
        |
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
      """trait ParserApi{
        |  def map: X
        |  /** */
        |  def flatMap: Y
        |}
      """.stripMargin
    )
    * - check(
      """object ThreadPoolConfig {
        |     { case _: SecurityException | 1 => None }
        |}
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
      """object X {
        |  val a = (1, 1)
        |
        |  ResultLogicalCombinators.mapMessage{}
        |}
      """.stripMargin
    )
    * - check(
      """
        |
        |object Go {
        |  trait Position extends Core
        |
        |  (1) match{ case x => x}
        |}
        |
      """.stripMargin
    )
    * - check(
      """
        |object build {
        |
        |  <developers>
        |    {map
        |    }
        |  </developers>
        |
        |
        |}
        |
      """.stripMargin
    )
    * - check(
      """
        |class MemAccess{
        |  def cond: Node
        |  inputs += addri
        |
        |}
        |
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
      """import com.twitter.scalding.macros.{ _ => _ }
      """.stripMargin
    )
    * - check(
      """object CyclicDependencyException {
        |  s"${info.resourceName}"
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
        """object immutable {
          |  new (((String)))
          |}
      """.stripMargin
    )
    * - check(
      """package immutable {
        |  class C extends (((D)))
        |}
      """.stripMargin
    )
    * - check(
      """class immutable {
        |  new ({val x = 1})
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
        |  @BeanProperty val x
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
      """class ScalaJavaCompletionProposalComputer {
        |  completionProposals: java.util.List[ICompletionProposal]
        |}
        |
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
      * - check(
        """class FunctionalBuilder{
          |  a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21), a22)
          |}
        """.stripMargin
      )
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
    * - check(
    """object X{
      |  // parses as ~?>.$(1)
      |  ~?>$ 1
      |}
    """.stripMargin
    )
    * - check(
      """object X{
        |  object Y{var z = 1}
        |  Y z = 2
        |  case class Z(z: Int){case class X(x: Int){ var y = 1}}
        |  Z apply 1 X 2 y = 4
        |
        |  a m "txt" t = 18; println("x")
        |  X x = 1 match {case _ => 2}
        |}
      """.stripMargin
    )
    * - check(
      """object X{
        |  type x = {def t: Int = 1}
        |}
      """.stripMargin
    )
    * - check(
      """object X{
        |  {x: Int => 1}
        |}
      """.stripMargin
    )
    * - check(// This should parse as a postfix call
      """object Foo{
        |  x_!toString
        |}
      """.stripMargin
    )


    // Pull these bits out and combine them to avoid escaping issues
    val slash = "\\"
    val u = "u"
    val d1 = "dca5"
    val d2 = "20ac"
    val d3 = "d83c"
    val d4 = "dca5"
    * - check(
      s"""object U{
        |  Seq(
        |    "$slash$u$d1",
        |    "$slash$u$d2"
        |  )
        |  Await.result(result, Duration.Inf) must be equalTo "$slash$u$d1$slash$u$d2$slash$u$d3"
        |}
      """.stripMargin
    )

    * - check(
      s"""object U{
        |  '$slash$u$d1'
        |  '$slash$u$d2'
        |}
      """.stripMargin
    )

    val o1 = "252"
    val o2 = "12"
    * - check(
      s"""
        |object X{
        |  println('$slash$o1')
        |  printl('$slash$o2')
        |}
      """.stripMargin
    )


    * - check(
      """
        |class Foo1 {
        |  trait Inner extends { val x : Int = 3 }
        |  class Inner extends { val x : Int = 3 }
        |}
      """.stripMargin
    )

    * - check(
      """class ScalaSdkData {
        |    <library type="Scala">
        |
        |    </library>
        |}
      """.stripMargin
    )
    * - check(
      """
        |class Foo {
        |  trait Inner <: { val x : Int = 3 }
        |}
        |class Bar extends Foo {
        |  trait Inner <: super.Inner { val y : Int = x }
        |}
      """.stripMargin
    )

    * - check(
      """class D extends {val x = 1} with C(2)
        |
        |trait B extends { val x = 1} with A { println("B") }
        |
      """.stripMargin
    )
    * - check(
      """object X{
        |  for(a <- "" ; (b, c) = (1, 2)) {}
        |}
      """.stripMargin
    )
    * - check(
      """object X{
        |(
        |1
        |+
        |)
        |}
      """.stripMargin
    )
    * - check(
      """class C{
        |  while (a
        |    && b) c
        |}
        |
      """.stripMargin
    )
    * - check(
      """class C{
        |  do x
        |  while(a
        |
        |  &&
        |  b)
        |}
        |
      """.stripMargin
    )
    * - check(
      """class C{
        |  x match{ case a @ _ ** y => z}
        |}
        |
      """.stripMargin
    )

    // These are currently illegal but parse correctly,
    // only failing later in the compiler
    * - check(
      s"""
        |object Test{
        |  t match {
        |    case Cell[a](x: Int) => ()
        |  }
        |}
      """.stripMargin
    )
    * - check(
      s"""trait T2 extends {
         |  type X = Int
         |} with Base
         |
      """.stripMargin
    )
    * - check(
      s"""
         |object ReflectiveCallTest {
         |    type ObjNotifyLike = {
         |      def notify: Unit
         |      def notify2: Unit
         |    }
         |
         |
         |}
         |
      """.stripMargin
    )
    * - check(
      """class Toto extends Expr with Case1(12);
        |
      """.stripMargin
    )
    * - check(
      """
        |class CallGraph{
        |  for {
        |      a     <- a
        |      b <- b
        |      c =  c
        |    } yield { }
        |}
      """.stripMargin
    )
    * - check(
      """class A extends B()""".stripMargin
    )
    * - check(
      """
        |object PopupDemo {
        |  contents += new RadioMenuItem
        |
        |  contents += 1
        |}""".stripMargin
    )
    * - check(
        """
        |object X {
        |  val foo: 23 = 23
        |  val foo: -23 = -23
        |  val foo: 23L = 23L
        |  val foo: -23L = -23L
        |  val foo: 23.0 = 23.0
        |  val foo: -23.0 = -23.0
        |  val foo: 23.0F = 23.0F
        |  val foo: -23.0F = -23.0F
        |  val foo: "foo" = "foo"
        |  val foo: 'f' = 'f'
        |  val foo: 'foo = 'foo
        |  val foo: true = true
        |}""".stripMargin
    )
    * - check(
        """
        |object X {
        |  val foo = bar[23]
        |  val foo = bar[-23]
        |  val foo = bar[23L]
        |  val foo = bar[-23L]
        |  val foo = bar[23.0]
        |  val foo = bar[-23.0]
        |  val foo = bar[23.0F]
        |  val foo = bar[-23.0F]
        |  val foo = bar["foo"]
        |  val foo = bar['f']
        |  val foo = bar['foo]
        |  val foo = bar[true]
        |}""".stripMargin
    )
    * - check(
        """
        |object X {
        |  def foo[T <: 23](t: T): T = ???
        |  def foo[T <: -23](t: T): T = ???
        |  def foo[T <: 23L](t: T): T = ???
        |  def foo[T <: -23L](t: T): T = ???
        |  def foo[T <: 23.0](t: T): T = ???
        |  def foo[T <: -23.0](t: T): T = ???
        |  def foo[T <: 23.0F](t: T): T = ???
        |  def foo[T <: -23.0F](t: T): T = ???
        |  def foo[T <: "foo"](t: T): T = ???
        |  def foo[T <: 'f'](t: T): T = ???
        |  def foo[T <: 'foo](t: T): T = ???
        |  def foo[T <: true](t: T): T = ???
        |}""".stripMargin
    )
  }
}
