package pythonparse

import utest._
import fastparse._
/**
 * Tests to cover most basic syntactic constructs. It's likely there are
 * interactions between the constructs that will cause problems, but these
 * are just to make sure that at a basic level each construct is supported.
 */
object UnitTests extends TestSuite{
  val tests = Tests {
    import Ast.expr._
    import Ast.stmt._
    import Ast.expr_context._
    import Ast.cmpop._
    import Ast.operator._
    import Ast.unaryop._
    import Ast._
    implicit def strName(s: Symbol) = Name(identifier(s.name), Load)
    implicit def strIdent(s: Symbol) = identifier(s.name)
    test("exprs"){
      def expr(expected: Ast.expr, s: String*) = s.map(TestUtils.check(Expressions.test(_), expected, _)).head


      test("primitives"){
        test("int") - expr(Num(1.0), "1")
        test("negative_int") - expr(Num(-1.0), "-1")
        test("float") - expr(Num(1.5), "1.5")
        test("negative_float") - expr(Num(-0.45), "-0.45")
        test("long") - expr(Num(10), "10L")
        test("long_lowercase") - expr(Num(25), "25l")
        test("negative_long") - expr(Num(-50), "-50L")
        test("emptyTuple") - expr(Tuple(Nil, Load), "()")
        test("name") - expr(Name(identifier("None"), Load), "None")
        test("yield") - expr(Yield(None), "(yield)")
        test("string") - expr(Str("Abc"), "'Abc'", "'Ab' b'c'")
      }
      test("operators"){
        test("math") - expr(BinOp(Num(1.0), Add, Num(2.0)), "1+2", "1 +  2")
        test("ident_math") - expr(
          BinOp(
            'a,
            operator.Add,
            'b
          ), "a + b")
        test("precedence") - expr(
          BinOp(
            BinOp(Num(1.0), Mult, Num(2.0)),
            Add,
            BinOp(Num(3.0), Div, Num(4.0))
          ),
          "1*2+3/4", "1 * 2  + 3  / 4"
        )
        test("unary") - expr(
          UnaryOp(
            Not,
            'a
          ),
          "not a"
        )
        test("unary2") - expr(
          UnaryOp(
            Not,
            UnaryOp(
              Not,
              'a
            )
          ),
          "not not a"
        )
        test("unary_invert") - expr(
          UnaryOp(
            Invert,
            'a
          ),
          "~a"
        )
        test("unary_negation") - expr(
          UnaryOp(
            USub,
            'b
          ),
          "-b"
        )
        test("unary_negative_number_negation") - expr(
          UnaryOp(
            USub,
            Num(-1)
          ),
          "--1"
        )
        test("unary_add") - expr(
          UnaryOp(
            UAdd,
            'c
          ),
          "+c"
        )
        test("unary_precedence") - expr(
          BinOp(
            BinOp(UnaryOp(USub, 'a), Add, 'b),
            Sub,
            'c
          ),
          "-a + b - c"
        )
        test("comparison") - expr(
          Compare(
            'a,
            Seq(Lt, LtE, Gt, GtE, Eq, NotEq, In, NotIn),
            Seq('b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
          ),
          "a < b <= c > d >= e == f != g in h not in i"
        )
        test("parenthetical_grouping") - expr(
          BinOp(BinOp('a, Add, 'b), Mult, BinOp('c, Sub, 'd)),
          "(a + b) * (c - d)"
        )
      }
      test("chained"){
        test("attributes") - expr(
          Attribute(Attribute('a, 'b, Load), 'c, Load),
          "a.b.c"
        )
        test("function_call") - expr(
          Call(
            Call(
              Call('a, Nil, Nil, None, None),
              Seq('x), Seq(keyword('y, 'z)), Some('wtf), Some('omg)
            ),
            Nil, Nil, None, Some('lol)
          ),
          "a()(x,y=z, *wtf, **omg)(**lol)"
        )
        test("slicing") - expr(
          Subscript('abc, slice.ExtSlice(Seq(slice.Index('d), slice.Slice(Some('e), Some('f), Some('None)))), Load),
          "abc[d, e:f:]"
        )
      }
      test("enclosed"){
        test("list") - expr(
          List(Seq(Num(1.0), Num(2.0), Str("a")), Load),
          "[1, 2, 'a']", "[1,2, 'a']"
        )
        test("list0") - expr(
          List(Seq(Num(1.0)), Load),
          "[1]", "[   1]"
        )
        test("tuple") - expr(
          Tuple(Seq(Num(1.0), Num(2.0), Str("a")), Load),
          "(1, 2, 'a')"
        )
        test("single_item_tuple") - expr(
          Tuple(Seq(Num(1.0)), Load),
          "(1,)"
        )
        test("set") - expr(
          Set(Seq(Num(1.0), Num(2.0), Str("a"))),
          "{1, 2, 'a'}"
        )
        test("set0") - expr(
          Set(Seq(Num(1.0))),
          "{1}"
        )
        test("dict") - expr(
          Dict(
            Seq(Num(1.0), Num(2.0), Str("a")),
            Seq(Str("1"), Str("2"), 'a)
          ),
          "{1 :'1', 2: '2', 'a': a}"
        )
        test("list_comp") - expr(
          ListComp('x, Seq(comprehension('y, 'z, Seq('w)))),
          "[x for y in z if w]"
        )

        test("list_comp2") - expr(
          ListComp(Tuple(Seq('x, 'y), Load), Seq(
            comprehension(
              Tuple(Seq('z, 'a), Load),
              Tuple(Seq('b, 'c), Load),
              Seq('d, 'e)
            ),
            comprehension('j, 'k, Nil)
          )),
          "[(x, y) for (z, a) in (b, c) if d if e for j in k]"
        )
        test("set_comp") - expr(
          SetComp('x, Seq(comprehension('y, 'z, Seq('w)))),
          "{x for y in z if w}"
        )
        test("dict_comp") - expr(
          DictComp('x, Num(1.0), Seq(comprehension('y, 'z, Seq('w)))),
          "{x: 1 for y in z if w}"
        )
        test("generator") - expr(
          GeneratorExp('x, Seq(comprehension('y, 'z, Seq('w)))),
          "(x for y in z if w)"
        )
      }
    }
    test("stmts"){
      def stmt(expected: Seq[Ast.stmt], s: String*) = s.map(TestUtils.check(Statements.file_input(_), expected, _)).head
      // Statements which only have expressions within them
      test("simple"){

        test("empty") - stmt(Nil, "")
        test("pass") - stmt(Seq(Pass), "pass")
        test("comment") - stmt(Seq(Pass), "pass\n#hello")
        test("trailing_newline") - stmt(Seq(Pass), "pass\n")
        test("expr") - stmt(Seq(Expr(Num(123))), "123")
        test("oneline") - stmt(Seq(Pass, Pass, Pass), "pass; pass; pass")
        test("twoline") - stmt(
          Seq(Pass, Pass),
          "pass\npass"
        )
        test("trailing_space") - stmt(
          Seq(Pass, Pass),
          "pass \npass"
        )
        test("pyramid") - stmt(
          Seq(
            Pass, Return(None), Return(Some(Num(1))),
            Delete(Seq('x)), Raise(Some('Foo), None, None),
            Assert('False, None)
          ),
          """pass; return; return 1;
            |del x; raise Foo
            |assert False;
          """.stripMargin
        )

        test("import") - stmt(
          Seq(Import(Seq(alias(identifier("a.b.c"), None)))),
          "import a.b.c"
        )
        test("import2") - stmt(
          Seq(Import(Seq(alias(identifier("a.b.c"), Some('d)), alias(identifier("e"), Some('f))))),
          "import a.b.c as d, e as f"
        )
        test("import3") - stmt(
          Seq(ImportFrom(Some('x), Seq(alias('y, None)), None)),
          "from x import y"
        )
        test("import4") - stmt(
          Seq(ImportFrom(Some(identifier("x.y")), Seq(alias('y, Some('z))), None)),
          "from x.y import y as z"
        )
        test("import5") - stmt(
          Seq(ImportFrom(Some(identifier("x.y")), Seq(alias('y, Some('z))), Some(1))),
          "from .x.y import y as z"
        )
        test("import6") - stmt(
          Seq(ImportFrom(None, Seq(alias('y, Some('z))), Some(2))),
          "from .. import y as z"
        )
        test("assign") - stmt(
          Seq(Assign(Seq(Name('x, Load)), Num(1))),
          "x = 1"
        )
        test("assign2") - stmt(
          Seq(Assign(Seq('x, Tuple(Seq('y, 'z), Load)), Num(1))),
          "x = y, z = 1"
        )
        test("augassign") - stmt(
          Seq(AugAssign('x, Add, Num(2))),
          "x += 2"
        )
      }
      // Statements which can have other statements within them
      test("compound"){
        test("while") - stmt(
          Seq(While('True, Seq(Pass), Nil)),
          """while True: pass"""
        )
        test("while2") - stmt(
          Seq(While('True, Seq(Pass, Pass), Nil)),
          """while True:
            |    pass
            |    pass
            |""".stripMargin
        )
        test("while3") - stmt(
          Seq(While('True, Seq(Expr(Call('func, Seq(Num(1)), Nil, None, None)), Pass), Nil), Pass),
          """while True:
            |    func(
            |1
            |    )
            |    pass
            |pass
            |""".stripMargin
        )
        test("for") - stmt(
          Seq(For(Tuple(Seq('x, 'y), Load), Call('range, Seq(Num(10)), Nil, None, None), Seq(Print(None, Seq('x), true)), Nil)),
          """for x, y in range(10):
            |  print x""".stripMargin
        )
        test("if") - stmt(
          Seq(If(
            'a,
            Seq(If(
              'b,
              Seq(Pass),
              Seq(Print(None, Seq(Num(1)), true))
            )),
            Seq(If(
              'c,
              Seq(Pass),
              Seq(If(
                'd,
                Seq(Pass),
                Seq(Pass)
              ))
            ))
          )),
          """if a:
            |  if b:
            |    pass
            |  else:
            |    print 1
            |else:
            |  if c: pass
            |  elif d: pass
            |  else: pass
          """.stripMargin
        )

        test("forelse") - stmt(
          Seq(For(Name('w, Load), Tuple(Seq('x, 'y, 'z), Load),
            Seq(For(Tuple(Seq('a, 'b), Load), 'c, Seq(Pass), Nil)),
            Seq(Pass)
          )),
          """for w in x, y, z:
            |  for a, b in c:
            |    pass
            |else:
            |  pass
          """.stripMargin
        )
        test("class1") - stmt(
          Seq(ClassDef('Foo, Nil, Seq(Pass), Nil)),
          """class Foo: pass""".stripMargin
        )
        test("class2") - stmt(
          Seq(ClassDef('Foo, Seq(BinOp('A, BitOr, 'B)), Seq(Pass), Seq('foo, Call(Attribute('bar, 'baz, Load), Seq(Num(1)), Nil, None, None)))),
          """@foo
            |@bar.baz(1)
            |class Foo(A | B):
            |   pass
          """.stripMargin
        )
        test("function") - stmt(
          Seq(FunctionDef('foo, arguments(Seq(Name('x, Param)), None, None, Nil), Seq(Return(Some('x))), Nil)),
          """def foo(x):
            |  return x
          """.stripMargin
        )
        test("function2") - stmt(
          Seq(FunctionDef(
            'foo,
            arguments(Seq(Name('x, Param), Name('y, Param)), None, Some('z), Seq(Num(1))),
            Seq(Return(Some('x))),
            Seq('dec)
          )),
          """@dec
            |def foo(x, y=1, **z):
            |  return x
          """.stripMargin
        )
        test("with") - stmt(
          Seq(With('x, Some(Name('y, Load)), Seq(Return(Some('y))))),
          "with x as y: return y"
        )
        test("with2") - stmt(
          Seq(With('x, Some(Name('y, Load)), Seq(With('a, Some(Name('b, Load)), Seq(Return(Some(Tuple(Seq('y, 'b), Load)))))))),
          "with x as y, a as b: return y, b"
        )
      }
    }
  }
}