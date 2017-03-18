package scalaparse.unit

import scalaparse.TestUtil._
import utest._

object TrailingCommasTests extends TestSuite {
  def tests = TestSuite {
    'pos {
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
        |""".stripMargin
      )
      'SimpleExpr1 - check("""
        |trait SimpleExpr1 {
        |  def f: (Int, String) = (
        |    23,
        |    "bar",
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
  }
}
