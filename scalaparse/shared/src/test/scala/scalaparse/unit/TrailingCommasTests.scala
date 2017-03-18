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

    'neg {
      // make sure trailing commas are only supported when multi-line
      'ArgumentExprs1 - checkNeg("""trait ArgumentExprs1 { f(23, "bar", )(Ev0, Ev1) }""")
      'ArgumentExprs2 - checkNeg("""trait ArgumentExprs2 { f(23, "bar")(Ev0, Ev1, ) }""")
      'ArgumentExprs3 - checkNeg("""trait ArgumentExprs3 { new C(23, "bar", )(Ev0, Ev1) }""")
      'ArgumentExprs4 - checkNeg("""trait ArgumentExprs4 { new C(23, "bar")(Ev0, Ev1, ) }""")

      'Params1 - checkNeg("""trait Params1 { def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1 }""")
      'Params2 - checkNeg("""trait Params2 { def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1 }""")
      'ClassParams1 - checkNeg("""trait ClassParams1 { final class C(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1) }""")
      'ClassParams2 - checkNeg("""trait ClassParams2 { final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, ) }""")

      'SimpleExpr - checkNeg("""trait SimpleExpr { (23, "bar", ) }""")

      'TypeArgs - checkNeg("""trait TypeArgs { def f: C[Int, String, ] }""")
      'TypeParamClause - checkNeg("""trait TypeParamClause { type C[A, B, ] }""")
      'FunTypeParamClause - checkNeg("""trait FunTypeParamClause { def f[A, B, ] }""")

      'SimpleType - checkNeg("""trait SimpleType { def f: (Int, String, ) }""")
      'FunctionArgTypes - checkNeg("""trait FunctionArgTypes { def f: (Int, String, ) => Boolean }""")

      'SimplePattern - checkNeg("""trait SimplePattern { val (foo, bar, ) = null: Any }""")

      'ImportSelectors - checkNeg("""trait ImportSelectors { import foo.{ Ev0, Ev1, } }""")

      'Import - checkNeg("""trait Import { import foo.Ev0, foo.Ev1, }""")

      'ValDcl - checkNeg("""trait ValDcl { val foo, bar, = 23 }""")
      'VarDcl - checkNeg("""trait VarDcl { var foo, bar, = 23 }""")
      'VarDef - checkNeg("""trait VarDef { var foo, bar, = _ }""")
      'PatDef - checkNeg("""trait PatDef { val Foo(foo), Bar(bar), = bippy }""")
    }
  }
}
