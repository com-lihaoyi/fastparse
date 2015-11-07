package pythonparse
import fastparse.noApi._
import WsApi._
import Expressions._
import acyclic.file
import Lexical.kw
object Statements extends Statements(0)
/**
 * Python's statement grammar. This can only be used in statement-blocks,
 * and is sensitive to newlines and indentation to determine nesting
 *
 * Manually transcribed from https://docs.python.org/2/reference/grammar.html
 */
class Statements(indent: Int){

  val space = P( CharIn(" \n") )
  val NEWLINE: P0 = P( "\n" | End )
  val ENDMARKER: P0 = P( End )

  val single_input: P[Seq[Ast.stmt]] = P(
    NEWLINE.map(_ => Nil) |
    simple_stmt |
    compound_stmt.map(Seq(_)) ~ NEWLINE
  )

  val indents = P( "\n" ~~ " ".repX(indent) )

  val spaces = P( (Lexical.nonewlinewscomment.? ~~ "\n").repX(1) )
  val file_input: P[Seq[Ast.stmt]] = P( spaces.? ~ stmt.repX(0, spaces) ~ spaces.? ).map(_.flatten)
  val eval_input: P[Ast.expr] = P( testlist ~ NEWLINE.rep ~ ENDMARKER ).map(tuplize)

  def collapse_dotted_name(name: Seq[Ast.identifier]): Ast.expr = {
    name.tail.foldLeft[Ast.expr](Ast.expr.Name(name.head, Ast.expr_context.Load))(
      (x, y) => Ast.expr.Attribute(x, y, Ast.expr_context.Load)
    )
  }

  val decorator: P[Ast.expr] = P( "@" ~/ dotted_name ~ ("(" ~ arglist ~ ")" ).?  ~~ Lexical.nonewlinewscomment.? ~~ NEWLINE).map{
    case (name, None) => collapse_dotted_name(name)
    case (name, Some((args, (keywords, starargs, kwargs)))) =>
      val x = collapse_dotted_name(name)
      Ast.expr.Call(x, args, keywords, starargs, kwargs)
  }

  val decorators = P( decorator.rep )
  val decorated: P[Ast.stmt] = P( decorators ~ (classdef | funcdef) ).map{case (a, b) => b(a)}
  val classdef: P[Seq[Ast.expr] => Ast.stmt.ClassDef] =
    P( kw("class") ~/ NAME ~ ("(" ~ testlist.? ~ ")").?.map(_.toSeq.flatten.flatten) ~ ":" ~~ suite ).map{
      case (a, b, c) => Ast.stmt.ClassDef(a, b, c, _)
    }


  val funcdef: P[Seq[Ast.expr] => Ast.stmt.FunctionDef] = P( kw("def") ~/ NAME ~ parameters ~ ":" ~~ suite ).map{
    case (name, args, suite) => Ast.stmt.FunctionDef(name, args, suite, _)
  }
  val parameters: P[Ast.arguments] = P( "(" ~ varargslist ~ ")" )

  val stmt: P[Seq[Ast.stmt]] = P( compound_stmt.map(Seq(_)) | simple_stmt )

  val simple_stmt: P[Seq[Ast.stmt]] = P( small_stmt.rep(1, sep = ";") ~ ";".? )
  val small_stmt: P[Ast.stmt] = P(
    print_stmt  | del_stmt | pass_stmt | flow_stmt |
    import_stmt | global_stmt | exec_stmt | assert_stmt | expr_stmt
  )
  val expr_stmt: P[Ast.stmt] = {
    val aug = P( testlist ~ augassign ~ (yield_expr | testlist.map(tuplize)) )
    val assign = P( testlist ~ ("=" ~ (yield_expr | testlist.map(tuplize))).rep )

    P(
      aug.map{case (a, b, c) => Ast.stmt.AugAssign(tuplize(a), b, c) } |
      assign.map{
        case (a, Nil) => Ast.stmt.Expr(tuplize(a))
        case (a, b) => Ast.stmt.Assign(Seq(tuplize(a)) ++ b.init, b.last)
      }
    )
  }

  val augassign: P[Ast.operator] = P(
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

  val print_stmt: P[Ast.stmt.Print] = {
    val noDest = P( test.rep(sep = ",") ~ ",".?).map(Ast.stmt.Print(None, _, true))
    val dest = P( ">>" ~ test ~ ("," ~ test).rep ~ ",".?).map{case (dest, exprs) => Ast.stmt.Print(Some(dest), exprs, true)}
    P( "print" ~~ " ".rep ~~ (noDest | dest) )
  }
  val del_stmt = P( kw("del") ~~ " ".rep ~~ exprlist ).map(Ast.stmt.Delete)
  val pass_stmt = P( kw("pass") ).map(_ => Ast.stmt.Pass)
  val flow_stmt: P[Ast.stmt] = P( break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt )
  val break_stmt = P( kw("break") ).map(_ => Ast.stmt.Break)
  val continue_stmt = P( kw("continue") ).map(_ => Ast.stmt.Continue)
  val return_stmt = P( kw("return") ~~ " ".rep ~~ testlist.map(tuplize).? ).map(Ast.stmt.Return)

  val yield_stmt = P( yield_expr ).map(Ast.stmt.Expr)
  val raise_stmt: P[Ast.stmt.Raise] = P( kw("raise") ~~ " ".rep ~~test.? ~ ("," ~ test).? ~ ("," ~ test).? ).map(Ast.stmt.Raise.tupled)
  val import_stmt: P[Ast.stmt] = P( import_name | import_from )
  val import_name: P[Ast.stmt.Import] = P( kw("import") ~ dotted_as_names ).map(Ast.stmt.Import)
  val import_from: P[Ast.stmt.ImportFrom] = {
    val named = P( ".".rep(1).!.? ~ dotted_name.!.map(Some(_)) )
    val unNamed = P( ".".rep(1).!.map(x => (Some(x), None)) )
    val star = P( "*".!.map(_ => Seq(Ast.alias(Ast.identifier("*"), None))) )
    P( kw("from") ~ (named | unNamed) ~ kw("import") ~ (star | "(" ~ import_as_names ~ ")" | import_as_names) ).map{
      case (dots, module, names) => Ast.stmt.ImportFrom(module.map(Ast.identifier), names, dots.map(_.length))
    }
  }
  val import_as_name: P[Ast.alias] = P( NAME ~ (kw("as") ~ NAME).? ).map(Ast.alias.tupled)
  val dotted_as_name: P[Ast.alias] = P( dotted_name.map(x => Ast.identifier(x.map(_.name).mkString("."))) ~ (kw("as") ~ NAME).? ).map(Ast.alias.tupled)
  val import_as_names = P( import_as_name.rep(1, ",") ~ (",").? )
  val dotted_as_names = P( dotted_as_name.rep(1, ",") )
  val dotted_name = P( NAME.rep(1, ".") )
  val global_stmt: P[Ast.stmt.Global] = P( kw("global") ~ NAME.rep(sep = ",") ).map(Ast.stmt.Global)
  val exec_stmt: P[Ast.stmt.Exec] = P( kw("exec") ~ expr ~ (kw("in") ~ test ~ ("," ~ test).?).? ).map {
    case (expr, None) => Ast.stmt.Exec(expr, None, None)
    case (expr, Some((globals, None))) => Ast.stmt.Exec(expr, Some(globals), None)
    case (expr, Some((globals, Some(locals)))) => Ast.stmt.Exec(expr, Some(globals), Some(locals))
  }
  val assert_stmt: P[Ast.stmt.Assert] = P( kw("assert") ~ test ~ ("," ~ test).? ).map(Ast.stmt.Assert.tupled)

  val compound_stmt: P[Ast.stmt] = P( if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | decorated )
  val if_stmt: P[Ast.stmt.If] = {
    val firstIf = P( kw("if") ~/ test ~ ":" ~~ suite )
    val elifs = P( (space_indents ~~ kw("elif") ~/ test ~ ":" ~~ suite).repX )
    val lastElse = P( (space_indents ~~ kw("else") ~/ ":" ~~ suite).? )
    P( firstIf ~~ elifs ~~ lastElse ).map{
      case (test, body, elifs, orelse) =>
        val (init :+ last) = (test, body) +: elifs
        val (last_test, last_body) = last
        init.foldRight(Ast.stmt.If(last_test, last_body, orelse.toSeq.flatten)){
          case ((test, body), rhs) => Ast.stmt.If(test, body, Seq(rhs))
        }
    }
  }
  val space_indents = P( spaces.repX ~~ " ".repX(indent) )
  val while_stmt = P( kw("while") ~/ test ~ ":" ~~ suite ~~ (space_indents ~~ kw("else") ~/ ":" ~~ suite).?.map(_.toSeq.flatten) ).map(Ast.stmt.While.tupled)
  val for_stmt: P[Ast.stmt.For] = P( kw("for") ~/ exprlist ~ kw("in") ~ testlist ~ ":" ~~ suite ~~ (space_indents ~ kw("else") ~/ ":" ~~ suite).? ).map {
    case (itervars, generator, body, orelse) =>
      Ast.stmt.For(tuplize(itervars), tuplize(generator), body, orelse.toSeq.flatten)
  }
  val try_stmt: P[Ast.stmt]= {
    val `try` = P( kw("try") ~/ ":" ~~ suite )
    val excepts: P[Seq[Ast.excepthandler]] = P( (except_clause ~ ":" ~~ suite).map{
      case (None, body) => Ast.excepthandler.ExceptHandler(None, None, body)
      case (Some((x, None)), body) => Ast.excepthandler.ExceptHandler(Some(x), None, body)
      case (Some((x, Some(y))), body) => Ast.excepthandler.ExceptHandler(Some(x), Some(y), body)
    }.repX )
    val `else` = P( space_indents ~~ kw("else") ~/ ":" ~~ suite )
    val `finally` = P( space_indents ~~ kw("finally") ~/ ":" ~~ suite )
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
  val with_stmt: P[Ast.stmt.With] = P( kw("with") ~/ with_item.rep(1, ",")~ ":" ~~ suite ).map{
    case (items, body) =>
      val (last_expr, last_vars) = items.last
      val inner = Ast.stmt.With(last_expr, last_vars, body)
      items.init.foldRight(inner){
        case ((expr, vars), body) => Ast.stmt.With(expr, vars, Seq(body))
      }
  }
  val with_item: P[(Ast.expr, Option[Ast.expr])] = P( test ~ (kw("as") ~ expr).? )
  // NB compile.c makes sure that the default except clause is last
  val except_clause = P( space_indents ~ kw("except") ~/ (test ~ ((kw("as") | ",") ~ test).?).? )


  val suite: P[Seq[Ast.stmt]] = {
    val deeper: P[Int] = {
      val commentLine = P("\n" ~~ Lexical.nonewlinewscomment.?.map(_ => 0)).map((_, Some("")))
      val endLine = P("\n" ~~ (" "|"\t").repX(indent + 1).!.map(_.length) ~~ Lexical.comment.!.? )
      P( Lexical.nonewlinewscomment.? ~~ ( endLine | commentLine ).repX(1) ).map{
        _.collectFirst{ case (s, None) => s}
      }.filter(_.isDefined).map(_.get)
    }
    val indented = P( deeper.flatMap{ nextIndent =>
      new Statements(nextIndent).stmt.repX(1, spaces.repX(1) ~~ (" " * nextIndent | "\t" * nextIndent)).map(_.flatten)
    } )
    P( indented | " ".rep ~ simple_stmt )
  }
}
