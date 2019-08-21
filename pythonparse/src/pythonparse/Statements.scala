package pythonparse

import fastparse._
import Expressions.{whitespace => _, _}
import Lexical.kw
object Statements extends Statements(0)
/**
 * Python's statement grammar. This can only be used in statement-blocks,
 * and is sensitive to newlines and indentation to determine nesting
 *
 * Manually transcribed from https://docs.python.org/2/reference/grammar.html
 */
class Statements(indent: Int){
  implicit def whitespace(cfg: P[_]): P[Unit] = Lexical.wscomment(cfg)
  def space[$: P] = P( CharIn(" \n") )
  def NEWLINE[$: P]: P0 = P( "\n" | End )
  def ENDMARKER[$: P]: P0 = P( End )

  def single_input[$: P]: P[Seq[Ast.stmt]] = P(
    NEWLINE.map(_ => Nil) |
    simple_stmt |
    compound_stmt.map(Seq(_)) ~ NEWLINE
  )

  def indents[$: P] = P( "\n" ~~ " ".repX(indent) )

  def spaces[$: P] = P( (Lexical.nonewlinewscomment.? ~~ "\n").repX(1) )
  def file_input[$: P]: P[Seq[Ast.stmt]] =
    P( spaces.? ~ stmt.repX(0, spaces) ~ spaces.? ).map(_.flatten)
  def eval_input[$: P]: P[Ast.expr] = P( testlist ~ NEWLINE.rep ~ ENDMARKER ).map(tuplize)

  def collapse_dotted_name(name: Seq[Ast.identifier]): Ast.expr = {
    name.tail.foldLeft[Ast.expr](Ast.expr.Name(name.head, Ast.expr_context.Load))(
      (x, y) => Ast.expr.Attribute(x, y, Ast.expr_context.Load)
    )
  }

  def decorator[$: P]: P[Ast.expr] = P( "@" ~/ dotted_name ~ ("(" ~ arglist ~ ")" ).?  ~~ Lexical.nonewlinewscomment.? ~~ NEWLINE).map{
    case (name, None) => collapse_dotted_name(name)
    case (name, Some((args, (keywords, starargs, kwargs)))) =>
      val x = collapse_dotted_name(name)
      Ast.expr.Call(x, args, keywords, starargs, kwargs)
  }

  def decorators[$: P] = P( decorator.rep )
  def decorated[$: P]: P[Ast.stmt] = P( decorators ~ (classdef | funcdef) ).map{case (a, b) => b(a)}
  def classdef[$: P]: P[Seq[Ast.expr] => Ast.stmt.ClassDef] =
    P( kw("class") ~/ NAME ~ ("(" ~ testlist.? ~ ")").?.map(_.toSeq.flatten.flatten) ~ ":" ~~ suite ).map{
      case (a, b, c) => Ast.stmt.ClassDef(a, b, c, _)
    }


  def funcdef[$: P]: P[Seq[Ast.expr] => Ast.stmt.FunctionDef] = P( kw("def") ~/ NAME ~ parameters ~ ":" ~~ suite ).map{
    case (name, args, suite) => Ast.stmt.FunctionDef(name, args, suite, _)
  }
  def parameters[$: P]: P[Ast.arguments] = P( "(" ~ varargslist ~ ")" )

  def stmt[$: P]: P[Seq[Ast.stmt]] = P( compound_stmt.map(Seq(_)) | simple_stmt )

  def simple_stmt[$: P]: P[Seq[Ast.stmt]] = P( small_stmt.rep(1, sep = ";") ~ ";".? )
  def small_stmt[$: P]: P[Ast.stmt] = P(
    print_stmt  | del_stmt | pass_stmt | flow_stmt |
    import_stmt | global_stmt | exec_stmt | assert_stmt | expr_stmt
  )
  def expr_stmt[$: P]: P[Ast.stmt] = {
    def aug = P( testlist ~ augassign ~ (yield_expr | testlist.map(tuplize)) )
    def assign = P( testlist ~ ("=" ~ (yield_expr | testlist.map(tuplize))).rep )

    P(
      aug.map{case (a, b, c) => Ast.stmt.AugAssign(tuplize(a), b, c) } |
      assign.map{
        case (a, Nil) => Ast.stmt.Expr(tuplize(a))
        case (a, b) => Ast.stmt.Assign(Seq(tuplize(a)) ++ b.init, b.last)
      }
    )
  }

  def augassign[$: P]: P[Ast.operator] = P(
    "+=".!.map(_ => Ast.operator.Add) |
    "-=".!.map(_ => Ast.operator.Sub) |
    "*=".!.map(_ => Ast.operator.Mult) |
    "/=".!.map(_ => Ast.operator.Div) |
    "%=".!.map(_ => Ast.operator.Mod) |
    "&=".!.map(_ => Ast.operator.BitAnd) |
    "|=".!.map(_ => Ast.operator.BitOr) |
    "^=".!.map(_ => Ast.operator.BitXor) |
    "<<=".!.map(_ => Ast.operator.LShift) |
    ">>=".!.map(_ => Ast.operator.RShift) |
    "**=".!.map(_ => Ast.operator.Pow) |
    "//=".!.map(_ => Ast.operator.FloorDiv)
  )

  def print_stmt[$: P]: P[Ast.stmt.Print] = {
    def noDest = P( test.rep(0, ",", Int.MaxValue, -1) ~ ",".?).map(Ast.stmt.Print(None, _, true))
    def dest = P( ">>" ~ test ~ ("," ~ test).rep ~ ",".?).map{case (dest, exprs) => Ast.stmt.Print(Some(dest), exprs, true)}
    P( "print" ~~ " ".rep ~~ (noDest | dest) )
  }
  def del_stmt[$: P] = P( kw("del") ~~ " ".rep ~~ exprlist ).map(Ast.stmt.Delete)
  def pass_stmt[$: P] = P( kw("pass") ).map(_ => Ast.stmt.Pass)
  def flow_stmt[$: P]: P[Ast.stmt] = P( break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt )
  def break_stmt[$: P] = P( kw("break") ).map(_ => Ast.stmt.Break)
  def continue_stmt[$: P] = P( kw("continue") ).map(_ => Ast.stmt.Continue)
  def return_stmt[$: P] = P( kw("return") ~~ " ".rep ~~ testlist.map(tuplize).? ).map(Ast.stmt.Return)

  def yield_stmt[$: P] = P( yield_expr ).map(Ast.stmt.Expr)
  def raise_stmt[$: P]: P[Ast.stmt.Raise] = P( kw("raise") ~~ " ".rep ~~test.? ~ ("," ~ test).? ~ ("," ~ test).? ).map(Ast.stmt.Raise.tupled)
  def import_stmt[$: P]: P[Ast.stmt] = P( import_name | import_from )
  def import_name[$: P]: P[Ast.stmt.Import] = P( kw("import") ~ dotted_as_names ).map(Ast.stmt.Import)
  def import_from[$: P]: P[Ast.stmt.ImportFrom] = {
    def named = P( ".".rep(1).!.? ~ dotted_name.!.map(Some(_)) )
    def unNamed = P( ".".rep(1).!.map(x => (Some(x), None)) )
    def star = P( "*".!.map(_ => Seq(Ast.alias(Ast.identifier("*"), None))) )
    P( kw("from") ~ (named | unNamed) ~ kw("import") ~ (star | "(" ~ import_as_names ~ ")" | import_as_names) ).map{
      case (dots, module, names) => Ast.stmt.ImportFrom(module.map(Ast.identifier), names, dots.map(_.length))
    }
  }
  def import_as_name[$: P]: P[Ast.alias] = P( NAME ~ (kw("as") ~ NAME).? ).map(Ast.alias.tupled)
  def dotted_as_name[$: P]: P[Ast.alias] = P( dotted_name.map(x => Ast.identifier(x.map(_.name).mkString("."))) ~ (kw("as") ~ NAME).? ).map(Ast.alias.tupled)
  def import_as_names[$: P] = P( import_as_name.rep(1, ",") ~ (",").? )
  def dotted_as_names[$: P] = P( dotted_as_name.rep(1, ",") )
  def dotted_name[$: P] = P( NAME.rep(1, ".") )
  def global_stmt[$: P]: P[Ast.stmt.Global] = P( kw("global") ~ NAME.rep(0, ",", Int.MaxValue, -1) ).map(Ast.stmt.Global)
  def exec_stmt[$: P]: P[Ast.stmt.Exec] = P( kw("exec") ~ expr ~ (kw("in") ~ test ~ ("," ~ test).?).? ).map {
    case (expr, None) => Ast.stmt.Exec(expr, None, None)
    case (expr, Some((globals, None))) => Ast.stmt.Exec(expr, Some(globals), None)
    case (expr, Some((globals, Some(locals)))) => Ast.stmt.Exec(expr, Some(globals), Some(locals))
  }
  def assert_stmt[$: P]: P[Ast.stmt.Assert] = P( kw("assert") ~ test ~ ("," ~ test).? ).map(Ast.stmt.Assert.tupled)

  def compound_stmt[$: P]: P[Ast.stmt] = P( if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | decorated )
  def if_stmt[$: P]: P[Ast.stmt.If] = {
    def firstIf = P( kw("if") ~/ test ~ ":" ~~ suite )
    def elifs = P( (space_indents ~~ kw("elif") ~/ test ~ ":" ~~ suite).repX )
    def lastElse = P( (space_indents ~~ kw("else") ~/ ":" ~~ suite).? )
    P( firstIf ~~ elifs ~~ lastElse ).map{
      case (test, body, elifs, orelse) =>
        val (init :+ last) = (test, body) +: elifs
        val (last_test, last_body) = last
        init.foldRight(Ast.stmt.If(last_test, last_body, orelse.toSeq.flatten)){
          case ((test, body), rhs) => Ast.stmt.If(test, body, Seq(rhs))
        }
    }
  }
  def space_indents[$: P] = P( spaces.repX ~~ " ".repX(indent) )
  def while_stmt[$: P] = P( kw("while") ~/ test ~ ":" ~~ suite ~~ (space_indents ~~ kw("else") ~/ ":" ~~ suite).?.map(_.toSeq.flatten) ).map(Ast.stmt.While.tupled)
  def for_stmt[$: P]: P[Ast.stmt.For] = P( kw("for") ~/ exprlist ~ kw("in") ~ testlist ~ ":" ~~ suite ~~ (space_indents ~ kw("else") ~/ ":" ~~ suite).? ).map {
    case (itervars, generator, body, orelse) =>
      Ast.stmt.For(tuplize(itervars), tuplize(generator), body, orelse.toSeq.flatten)
  }
  def try_stmt[$: P]: P[Ast.stmt]= {
    def `try` = P( kw("try") ~/ ":" ~~ suite )
    def excepts: P[Seq[Ast.excepthandler]] = P( (except_clause ~ ":" ~~ suite).map{
      case (None, body) => Ast.excepthandler.ExceptHandler(None, None, body)
      case (Some((x, None)), body) => Ast.excepthandler.ExceptHandler(Some(x), None, body)
      case (Some((x, Some(y))), body) => Ast.excepthandler.ExceptHandler(Some(x), Some(y), body)
    }.repX )
    def `else` = P( space_indents ~~ kw("else") ~/ ":" ~~ suite )
    def `finally` = P( space_indents ~~ kw("finally") ~/ ":" ~~ suite )
    P( `try` ~~ excepts ~~ `else`.? ~~ `finally`.? ).map{
      case (tryBlock, excepts, elseBlock, None) =>
        Ast.stmt.TryExcept(tryBlock, excepts, elseBlock.toSeq.flatten)
      case (tryBlock, Nil, None, Some(finallyBlock)) =>
        Ast.stmt.TryFinally(tryBlock, finallyBlock)
      case (tryBlock, excepts, elseBlock, Some(finallyBlock)) =>
        Ast.stmt.TryFinally(
          Seq(Ast.stmt.TryExcept(tryBlock, excepts, elseBlock.toSeq.flatten)),
          finallyBlock
        )
    }
  }
  def with_stmt[$: P]: P[Ast.stmt.With] = P( kw("with") ~/ with_item.rep(1, ",")~ ":" ~~ suite ).map{
    case (items, body) =>
      val (last_expr, last_vars) = items.last
      val inner = Ast.stmt.With(last_expr, last_vars, body)
      items.init.foldRight(inner){
        case ((expr, vars), body) => Ast.stmt.With(expr, vars, Seq(body))
      }
  }
  def with_item[$: P]: P[(Ast.expr, Option[Ast.expr])] = P( test ~ (kw("as") ~ expr).? )
  // NB compile.c makes sure that the default except clause is last
  def except_clause[$: P] = P( space_indents ~ kw("except") ~/ (test ~ ((kw("as") | ",") ~ test).?).? )


  def suite[$: P]: P[Seq[Ast.stmt]] = {
    def deeper: P[Int] = {
      def commentLine = P("\n" ~~ Lexical.nonewlinewscomment.?.map(_ => 0)).map((_, Some("")))
      def endLine = P("\n" ~~ (" "|"\t").repX(indent + 1).!.map(_.length) ~~ Lexical.comment.!.? )
      P( Lexical.nonewlinewscomment.? ~~ ( endLine | commentLine ).repX(1) ).map{
        _.collectFirst{ case (s, None) => s}
      }.filter(_.isDefined).map(_.get)
    }
    def indented = P( deeper.flatMapX{ nextIndent =>
      new Statements(nextIndent).stmt.repX(1, spaces.repX(1) ~~ (" " * nextIndent | "\t" * nextIndent)).map(_.flatten)
    } )
    P( indented | " ".rep ~ simple_stmt )
  }
}
