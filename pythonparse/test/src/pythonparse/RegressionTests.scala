package pythonparse
import utest._
import fastparse._

object RegressionTests extends TestSuite{
  import Ast.expr._
  import Ast.stmt._
  import Ast.expr_context._
  import Ast.cmpop._
  import Ast.operator._
  import Ast.unaryop._
  import Ast._
  implicit def strName(s: Symbol): Name = Name(identifier(s.name), Load)
  implicit def strIdent(s: Symbol): identifier = identifier(s.name)
  val tests = Tests {
    test("multiple_comments") - TestUtils.check(
      Statements.file_input(_),
      Seq(Ast.stmt.Pass),
      """# a
        |# b
        |pass""".stripMargin
    )

    test("multiple_newlines") - TestUtils.check(
      Statements.file_input(_),
      Seq(Expr(Symbol("a")), Expr(Symbol("b"))),
      """a
        |
        |b""".stripMargin
    )

    test("multi_line_function") - TestUtils.check(
      Statements.file_input(_),
      Seq(FunctionDef(Symbol("session_config"), arguments(Nil, None, None, Nil), Seq(Expr(Symbol("a")), Expr(Symbol("b"))), Nil)),
      """def session_config():
        |    a
        |
        |    b""".stripMargin
    )

    test("backslash_breaks") - TestUtils.check(
      Statements.file_input(_),
      Seq(Expr(Attribute(Symbol("a"), Symbol("b"), Load))),
      """a\
        |.b
        |""".stripMargin
    )
    test("multiline_string") - TestUtils.check(
      Statements.file_input(_),
      Seq(Expr(Str("\n"))),
      """'''
        |'''
        |""".stripMargin
    )

    test("try_finally_no_except") - TestUtils.check(
      Statements.file_input(_),
      Seq(TryFinally(Seq(Expr(Symbol("a"))), Seq(Expr(Symbol("b"))))),
      """try:
        |    a
        |finally:
        |    b
        |
        |""".stripMargin
    )
    test("indented_try_except_with_space") - TestUtils.check(
      Statements.file_input(_),
      Seq(FunctionDef(Symbol("f"), arguments(Nil, None, None, Nil), Seq(
        TryExcept(
          Seq(Pass),
          Seq(excepthandler.ExceptHandler(Some(Symbol("s")), None, Seq(Pass))),
          Nil
        )
      ), Nil)),
      """def f():
        |    try:
        |        pass
        |
        |    except s:
        |        pass
        |
        |""".stripMargin
    )
    test("indented_block_with_spaces_and_offset_comments") - TestUtils.check(
      Statements.file_input(_),
      Seq(FunctionDef(
        Symbol("post"),
        arguments(Seq(Name(Symbol("self"), Param)), None, None, Nil),
        Seq(If(Num(1), Seq(Expr(Symbol("a"))), Nil)),
        Nil
      )),
      """def post(self):
        |#LOL
        |
        |    if 1:
        |        a
        |""".stripMargin
    )
    test("indented_block_with_spaces_and_offset_comments") - TestUtils.check(
      Statements.file_input(_),
      Seq(While(
        Symbol("a"),
        Seq(
          TryExcept(
            Seq(Expr(Symbol("a"))),
            Seq(
              excepthandler.ExceptHandler(None, None, Seq(Return(Some(Symbol("a"))))),
              excepthandler.ExceptHandler(None, None, Seq(Expr(Symbol("a"))))
            ),
            Nil
          )
        ),
        Nil
      )),
      """while a:
        |    try: a
        |    except: return a
        |    except: a
        |""".stripMargin
    )

    test("weird_comments") - TestUtils.check(
      Statements.file_input(_),
      Seq(While(
        Num(1),
        Seq(Expr(Symbol("a"))),
        Nil
      )),
      """while 1:
        |    #
        |    a
        |""".stripMargin
    )
    test("ident_looking_string") - TestUtils.check(
      Statements.file_input(_),
      Seq(If(
        Call(Symbol("match"), Seq(Str("^[a-zA-Z0-9]")), Nil, None, None),
        Seq(Expr(Symbol("a"))),
        Nil
      )),
      """
        |if match(r'^[a-zA-Z0-9]'):
        |    a
        |
        |""".stripMargin
    )
    test("same_line_comment") - TestUtils.check(
      Statements.file_input(_),
      Seq(If(
        Symbol("b"),
        Seq(If(
          Symbol("c"),
          Seq(Pass),
          Nil
        )),
        Nil
      )),
      """if b:  #
        |    if c:
        |        pass
        |""".stripMargin
    )
    test("chained_elifs") - TestUtils.check(
      Statements.file_input(_),
      Seq(While(Num(1),
        Seq(
          If(Symbol("a"),
            Seq(Expr(Symbol("a"))),
            Seq(
              If(Symbol("b"),
                Seq(Expr(Symbol("b"))),
                Seq(If(Symbol("c"),
                  Seq(Expr(Symbol("c"))),
                  Nil
                )))
            ))
        ),
        Nil
      )),
      """while 1:
        |    if a:
        |        a
        |    elif b:
        |        b
        |    elif c:
        |        c
        |""".stripMargin
    )
    test("bitand") - TestUtils.check(
      Statements.file_input(_),
      Seq(Expr(BinOp(Symbol("a"), BitAnd, Symbol("a")))),
      """a & a
        |""".stripMargin
    )
    test("octal") - TestUtils.check(
      Statements.file_input(_),
      Seq(Expr(Num(0))),
      """0x0
        |""".stripMargin
    )
    test("comment_after_decorator") - TestUtils.check(
      Statements.file_input(_),
      Seq(ClassDef(Symbol("GenericForeignKeyTests"), Nil, Seq(Pass), Seq(Symbol("override_settings")))),
      """@override_settings # ForeignKey(unique=True)
        |class GenericForeignKeyTests:
        |    pass
        |""".stripMargin
    )
    test("while_block") - TestUtils.check(
      Statements.file_input(_),
      Seq(While(Num(1), Seq(Expr(Num(2)), Expr(Num(3))), Nil)),
      """while 1:
        |
        |    2
        |    3
        |""".stripMargin
    )
    test("while_in_if") - TestUtils.check(
      Statements.file_input(_),
      Seq(If(Num(1), Seq(While(Num(0), Seq(Pass), Nil)), Seq(Pass))),
      """if 1:
        |    while 0:
        |        pass
        |else:
        |    pass
        |""".stripMargin
    )

    test("tab_indent") - TestUtils.check(
      Statements.file_input(_),
      Seq(While(Num(1), Seq(Pass), Nil)),
      "while 1:\n\tpass"
    )

    test("negative_integer") - TestUtils.check(
      Statements.file_input(_),
      Seq(Expr(Num(-1))),
      "-1"
    )
    test("unary_subtraction") - TestUtils.check(
      Statements.file_input(_),
      Seq(Expr(UnaryOp(USub, Name(identifier("foo"), Load)))),
      "-foo"
    )
  }
}

