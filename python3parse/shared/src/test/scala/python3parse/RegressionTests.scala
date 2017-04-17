package python3parse
import utest._
import fastparse.all._

object RegressionTests extends TestSuite{
  import Ast.expr._
  import Ast.stmt._
  import Ast.expr_context._
  import Ast.cmpop._
  import Ast.operator._
  import Ast.unaryop._
  import Ast._
  implicit def strName(s: Symbol) = Name(identifier(s.name), Load)
  implicit def strIdent(s: Symbol) = identifier(s.name)
  implicit def symArg(s: Symbol) = arg(identifier(s.name))
  val tests = TestSuite{
    'multiple_comments - TestUtils.check(
      Statements.file_input,
      Seq(Ast.stmt.Pass),
      """# a
        |# b
        |pass""".stripMargin
    )

    'multiple_newlines - TestUtils.check(
      Statements.file_input,
      Seq(Expr('a), Expr('b)),
      """a
        |
        |b""".stripMargin
    )

    'multi_line_function - TestUtils.check(
      Statements.file_input,
      Seq(FunctionDef('session_config, arguments.empty, Seq(Expr('a), Expr('b)), Nil, None)),
      """def session_config():
        |    a
        |
        |    b""".stripMargin
    )

    'backslash_breaks - TestUtils.check(
      Statements.file_input,
      Seq(Expr(Attribute('a, 'b, Load))),
      """a\
        |.b
        |""".stripMargin
    )
    'multiline_string - TestUtils.check(
      Statements.file_input,
      Seq(Expr(Str("\n"))),
      """'''
        |'''
        |""".stripMargin
    )

    'try_finally_no_except - TestUtils.check(
      Statements.file_input,
      Seq(Try(Seq(Expr('a)), Nil, Nil, Seq(Expr('b)))),
      """try:
        |    a
        |finally:
        |    b
        |
        |""".stripMargin
    )
    'indented_try_except_with_space - TestUtils.check(
      Statements.file_input,
      Seq(FunctionDef('f, arguments.empty, Seq(
        Try(
          Seq(Pass),
          Seq(excepthandler.ExceptHandler(Some('s), None, Seq(Pass))),
          Nil,
          Nil
        )
      ), Nil, None)),
      """def f():
        |    try:
        |        pass
        |
        |    except s:
        |        pass
        |
        |""".stripMargin
    )
    'indented_block_with_spaces_and_offset_comments - TestUtils.check(
      Statements.file_input,
      Seq(FunctionDef(
        'post,
        arguments(Seq('self), None, Nil, Nil, None, Nil),
        Seq(If(Num(1), Seq(Expr('a)), Nil)),
        Nil,
        None
      )),
      """def post(self):
        |#LOL
        |
        |    if 1:
        |        a
        |""".stripMargin
    )
    'indented_block_with_spaces_and_offset_comments - TestUtils.check(
      Statements.file_input,
      Seq(While(
        'a,
        Seq(
          Try(
            Seq(Expr('a)),
            Seq(
              excepthandler.ExceptHandler(None, None, Seq(Return(Some('a)))),
              excepthandler.ExceptHandler(None, None, Seq(Expr('a)))
            ),
            Nil,
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

    'weird_comments - TestUtils.check(
      Statements.file_input,
      Seq(While(
        Num(1),
        Seq(Expr('a)),
        Nil
      )),
      """while 1:
        |    #
        |    a
        |""".stripMargin
    )
    'ident_looking_string - TestUtils.check(
      Statements.file_input,
      Seq(If(
        Call('match, Seq(Str("^[a-zA-Z0-9]")), Nil),
        Seq(Expr('a)),
        Nil
      )),
      """
        |if match(r'^[a-zA-Z0-9]'):
        |    a
        |
        |""".stripMargin
    )
    'same_line_comment - TestUtils.check(
      Statements.file_input,
      Seq(If(
        'b,
        Seq(If(
          'c,
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
    'chained_elifs - TestUtils.check(
      Statements.file_input,
      Seq(While(Num(1),
        Seq(
          If('a,
            Seq(Expr('a)),
            Seq(
              If('b,
                Seq(Expr('b)),
                Seq(If('c,
                  Seq(Expr('c)),
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
    'bitand - TestUtils.check(
      Statements.file_input,
      Seq(Expr(BinOp('a, BitAnd, 'a))),
      """a & a
        |""".stripMargin
    )
    'octal - TestUtils.check(
      Statements.file_input,
      Seq(Expr(Num(0))),
      """0x0
        |""".stripMargin
    )
    'comment_after_decorator - TestUtils.check(
      Statements.file_input,
      Seq(ClassDef('GenericForeignKeyTests, Nil, Nil, Seq(Pass), Seq('override_settings))),
      """@override_settings # ForeignKey(unique=True)
        |class GenericForeignKeyTests:
        |    pass
        |""".stripMargin
    )
    'while_block - TestUtils.check(
      Statements.file_input,
      Seq(While(Num(1), Seq(Expr(Num(2)), Expr(Num(3))), Nil)),
      """while 1:
        |
        |    2
        |    3
        |""".stripMargin
    )
    'while_in_if - TestUtils.check(
      Statements.file_input,
      Seq(If(Num(1), Seq(While(Num(0), Seq(Pass), Nil)), Seq(Pass))),
      """if 1:
        |    while 0:
        |        pass
        |else:
        |    pass
        |""".stripMargin
    )

    'tab_indent - TestUtils.check(
      Statements.file_input,
      Seq(While(Num(1), Seq(Pass), Nil)),
      "while 1:\n\tpass"
    )

    'negative_integer - TestUtils.check(
      Statements.file_input,
      Seq(Expr(Num(-1))),
      "-1"
    )
    'unary_subtraction - TestUtils.check(
      Statements.file_input,
      Seq(Expr(UnaryOp(USub, Name(identifier("foo"), Load)))),
      "-foo"
    )
  }
}

