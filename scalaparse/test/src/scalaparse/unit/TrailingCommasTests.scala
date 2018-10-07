package scalaparse.unit

import scalaparse.TestUtil._
import utest._

object TrailingCommasTests extends TestSuite {
  def tests = TestSuite {
    'pos - {
      'ArgumentExprs1 - check("""
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
      'ArgumentExprs2 - check("""
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
      'Params - check("""
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
      'ClassParams - check("""
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
      'SimpleExpr1 - check("""
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
      'TypeArgs - check("""
        |trait TypeArgs {
        |  class C[A, B]
        |  def f: C[
        |    Int,
        |    String,
        |  ]
        |}""".stripMargin
      )
      'TypeParamClause - check("""
        |class C[
        |  A,
        |  B,
        |]""".stripMargin
      )
      'FunTypeParamClause - check("""
        |trait FunTypeParamClause {
        |  def f[
        |    A,
        |    B,
        |  ]
        |}""".stripMargin
      )
      'SimpleType - check("""
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
      'FunctionArgTypes - check("""
        |trait FunctionArgTypes {
        |  def f: (
        |    Int,
        |    String,
        |  ) => Boolean
        |}""".stripMargin
      )
      'SimplePattern - check("""
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
      'ImportSelectors - check(
        """
        |import foo.{
        |  Ev0,
        |  Ev1,
        |}""".stripMargin
      )
      'Bindings - check(
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

    'neg - {
      //// Multi-line only cases: make sure trailing commas are only supported when multi-line

      'ArgumentExprs1 - checkNeg("""trait ArgumentExprs1 { f(23, "bar", )(Ev0, Ev1) }""", expected1, found1)
      'ArgumentExprs2 - checkNeg("""trait ArgumentExprs2 { f(23, "bar")(Ev0, Ev1, ) }""", expected1, found1)
      'ArgumentExprs3 - checkNeg("""trait ArgumentExprs3 { new C(23, "bar", )(Ev0, Ev1) }""", expected1, found1)
      'ArgumentExprs4 - checkNeg("""trait ArgumentExprs4 { new C(23, "bar")(Ev0, Ev1, ) }""", expected1, found1)

      'Params1 - checkNeg("""trait Params1 { def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1) = 1 }""", expected1, found1)
      'Params2 - checkNeg("""trait Params2 { def f(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, ) = 1 }""", expected1, found1)
      'ClassParams1 - checkNeg("""final class C(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1)""", expected1, found1)
      'ClassParams2 - checkNeg("""final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, )""", expected1, found1)

      'SimpleExpr - checkNeg("""trait SimpleExpr { (23, "bar", ) }""", expected1, found1)

      'TypeArgs - checkNeg("""trait TypeArgs { def f: C[Int, String, ] }""", expected2, found2)
      'TypeParamClause - checkNeg("""class C[A, B, ] }""", expected2, found2)
      'FunTypeParamClause - checkNeg("""trait FunTypeParamClause { def f[A, B, ] }""", expected2, found2)

      'SimpleType - checkNeg("""trait SimpleType { def f: (Int, String, ) }""", expected1, found1)
      'FunctionArgTypes - checkNeg("""trait FunctionArgTypes { def f: (Int, String, ) => Boolean }""", expected1, found1)

      'SimplePattern - checkNeg("""trait SimplePattern { val (foo, bar, ) = null: Any }""", expected1, found1)

      'ImportSelectors - checkNeg("""import foo.{ Ev0, Ev1, }""", expected3, found3)

      'Import - checkNeg("""trait Import { import foo.Ev0, foo.Ev1, }""", expected4, found4)

      'ValDcl - checkNeg("""trait ValDcl { val foo, bar, = 23 }""", expected5, found5)
      'VarDcl - checkNeg("""trait VarDcl { var foo, bar, = 23 }""", expected5, found5)
      'VarDef - checkNeg("""trait VarDef { var foo, bar, = _ }""", expected5, found5)
      'PatDef - checkNeg("""trait PatDef { val Foo(foo), Bar(bar), = bippy }""", expected5, found5)

      //// The Tuple 1 cases

      // the Tuple1 value case: make sure that the possible "(23, )" syntax for Tuple1 doesn't compile to "23"
      "Tuple1 value case" - checkNeg("trait SimpleExpr2 { (23, ) }", expected1, found1)

      // the Tuple1 type case: make sure that the possible "(Int, )" syntax for Tuple1[Int] doesn't compile to "Int"
      "Tuple1 type case" - checkNeg("trait SimpleType2 { def f: (Int, ) }", expected1, found1)

      val expected1 = """WSChars | Comment | Newline | ")""""
      val expected2 = """WSChars | Comment | Newline | "]""""
      val expected3 = """WSChars | Comment | Newline | "}""""
      val expected4 = "ThisPath | IdPath"
      val expected5 = "Binding ~ InfixPattern | InfixPattern | VarId"
      val found1 = ", )"
      val found2 = ", ]"
      val found3 = ", }"
      val found4 = "}"
      val found5 = "="
    }
  }
}
