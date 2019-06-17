package scalaparse.unit

import scalaparse.TestUtil._
import utest._

object TrailingCommasTests extends TestSuite {
  def tests = TestSuite {
    test("pos"){
      test("ArgumentExprs1") - check("""
        |trait ArgumentExprs1 {
        |  def f(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1) = 1
        |  f(
        |    23,
        |    "bar",
        |  )(
        |    Ev0,
        |    Ev1,
        |  )
        |
        |  // test arg exprs in the presence of varargs
        |  def g(x: Int, y: Int*) = 1
        |  g(1,2,
        |  )
        |  g(1,List(2, 3): _*,
        |  )
        |}""".stripMargin
      )
      test("ArgumentExprs2") - check("""
        |trait ArgumentExprs2 {
        |  class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1)
        |  new C(
        |    23,
        |    "bar",
        |  )(
        |    Ev0,
        |    Ev1,
        |  )
        |}""".stripMargin
      )
      test("Params") - check("""
        |trait Params {
        |  def f(
        |    foo: Int,
        |    bar: String,
        |  )(implicit
        |    ev0: Ev0,
        |    ev1: Ev1,
        |  )
        |}""".stripMargin
      )
      test("ClassParams") - check("""
        |class C(
        |  foo: Int,
        |  bar: String,
        |)(implicit
        |  ev0: Ev0,
        |  ev1: Ev1,
        |)
        |
        |// test class params in the presence of varargs
        |case class D(i: Int*,
        |)
        |
        |// test you can have comments after the last parameter
        |class E(
        |  foo: Int, // foo is the start of "foo-bar"
        |)
        |""".stripMargin
      )
      test("SimpleExpr1") - check("""
        |trait SimpleExpr1 {
        |  def f: (Int, String) = (
        |    23,
        |    "bar",
        |  )
        |
        |  // the Tuple1 value case, the trailing comma is ignored so the type is Int and the value 23
        |  def g: Int = (
        |    23,
        |  )
        |}""".stripMargin
      )
      test("TypeArgs") - check("""
        |trait TypeArgs {
        |  class C[A, B]
        |  def f: C[
        |    Int,
        |    String,
        |  ]
        |}""".stripMargin
      )
      test("TypeParamClause") - check("""
        |class C[
        |  A,
        |  B,
        |]""".stripMargin
      )
      test("FunTypeParamClause") - check("""
        |trait FunTypeParamClause {
        |  def f[
        |    A,
        |    B,
        |  ]
        |}""".stripMargin
      )
      test("SimpleType") - check("""
        |trait SimpleType {
        |  def f: (
        |    Int,
        |    String,
        |  )
        |
        |  // the Tuple1 type case, the trailing comma is ignored so the type is Int and the value 23
        |  def g: (
        |    Int,
        |  ) = 23
        |}""".stripMargin
      )
      test("FunctionArgTypes") - check("""
        |trait FunctionArgTypes {
        |  def f: (
        |    Int,
        |    String,
        |  ) => Boolean
        |}""".stripMargin
      )
      test("SimplePattern") - check("""
        |trait SimplePattern {
        |  val (
        |    foo,
        |    bar,
        |  ) = null: Any
        |
        |  // test '@' syntax in patterns
        |  Some(1) match {
        |    case Some(x @ 1,
        |    ) => x
        |  }
        |
        |  // test ': _*' syntax in patterns
        |  List(1, 2, 3) match {
        |    case List(1, 2, _ @ _*,
        |    ) => 1
        |  }
        |
        |  // test varargs in patterns
        |  val List(x, y, _*,
        |  ) = 42 :: 17 :: Nil
        |}""".stripMargin
      )
      test("ImportSelectors") - check(
        """
        |import foo.{
        |  Ev0,
        |  Ev1,
        |}""".stripMargin
      )
      test("Bindings") - check(
        """
        |trait Bindings {
        |  def g(f: (Int, String) => Boolean)
        |
        |  g((
        |    foo,
        |    bar,
        |  ) => true)
        |}""".stripMargin
      )
      // Import, ids, ValDcl, VarDcl, VarDef, PatDef use commas, but not inside paren, bracket or brace,
      // so they don't support an optional trailing comma
    }

    test("neg"){
      //// Multi-line only cases: make sure trailing commas are only supported when multi-line

      test("ArgumentExprs1") - checkNeg("""trait ArgumentExprs1 { f(23, "bar", )(Ev0, Ev1) }""", null, null, found1)
      test("ArgumentExprs2") - checkNeg("""trait ArgumentExprs2 { f(23, "bar")(Ev0, Ev1, ) }""", null, null, found1)
      test("ArgumentExprs3") - checkNeg("""trait ArgumentExprs3 { new C(23, "bar", )(Ev0, Ev1) }""", null, null, found1)
      test("ArgumentExprs4") - checkNeg("""trait ArgumentExprs4 { new C(23, "bar")(Ev0, Ev1, ) }""", null, null, found1)

      test("Params1") - checkNeg("""trait Params1 { def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1) = 1 }""", null, null, found1)
      test("Params2") - checkNeg("""trait Params2 { def f(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, ) = 1 }""", null, null, found1)
      test("ClassParams1") - checkNeg("""final class C(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1)""", null, null, found1)
      test("ClassParams2") - checkNeg("""final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, )""", null, null, found1)

      test("SimpleExpr") - checkNeg("""trait SimpleExpr { (23, "bar", ) }""", null, null, found1)

      test("TypeArgs") - checkNeg("""trait TypeArgs { def f: C[Int, String, ] }""", null, null, found2)
      test("TypeParamClause") - checkNeg("""class C[A, B, ] }""", null, null, found2)
      test("FunTypeParamClause") - checkNeg("""trait FunTypeParamClause { def f[A, B, ] }""", null, null, found2)

      test("SimpleType") - checkNeg("""trait SimpleType { def f: (Int, String, ) }""", null, null, found1)
      test("FunctionArgTypes") - checkNeg("""trait FunctionArgTypes { def f: (Int, String, ) => Boolean }""", null, null, found1)

      test("SimplePattern") - checkNeg("""trait SimplePattern { val (foo, bar, ) = null: Any }""", null, null, found1)

      test("ImportSelectors") - checkNeg("""import foo.{ Ev0, Ev1, }""", null, null, found3)

      test("Import") - checkNeg("""trait Import { import foo.Ev0, foo.Ev1, }""", null, null, found4)

      test("ValDcl") - checkNeg("""trait ValDcl { val foo, bar, = 23 }""", null, null, found5)
      test("VarDcl") - checkNeg("""trait VarDcl { var foo, bar, = 23 }""", null, null, found5)
      test("VarDef") - checkNeg("""trait VarDef { var foo, bar, = _ }""", null, null, found5)
      test("PatDef") - checkNeg("""trait PatDef { val Foo(foo), Bar(bar), = bippy }""", null, null, found5)

      //// The Tuple 1 cases

      // the Tuple1 value case: make sure that the possible "(23, )" syntax for Tuple1 doesn't compile to "23"
      "Tuple1 value case" - checkNeg("trait SimpleExpr2 { (23, ) }", null, null, found1)

      // the Tuple1 type case: make sure that the possible "(Int, )" syntax for Tuple1[Int] doesn't compile to "Int"
      "Tuple1 type case" - checkNeg("trait SimpleType2 { def f: (Int, ) }", null, null, found1)

      val found1 = ", )"
      val found2 = ", ]"
      val found3 = ", }"
      val found4 = "}"
      val found5 = "="
    }
  }
}
