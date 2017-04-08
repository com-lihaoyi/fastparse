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
 * Manually transcribed from https://docs.python.org/3/reference/grammar.html
 */
class Statements(indent: Int){

  val space = P( CharIn(" \n") )
  val NEWLINE: P0 = P( "\n" | End )
  val ENDMARKER: P0 = P( End )

  /// single_input: NEWLINE | simple_stmt | compound_stmt
  val single_input: P[Seq[Ast.stmt]] = P(
    NEWLINE.map(_ => Nil) |
    simple_stmt |
    compound_stmt.map(Seq(_)) ~ NEWLINE
  )

  val indents = P( "\n" ~~ " ".repX(indent) )

  val spaces = P( (Lexical.nonewlinewscomment.? ~~ "\n").repX(1) )

  /// file_input: (NEWLINE | stmt)* ENDMARKER
  val file_input: P[Seq[Ast.stmt]] = P( spaces.? ~ stmt.repX(0, spaces) ~ spaces.? ).map(_.flatten)

  /// eval_input: testlist NEWLINE* ENDMARKER
  val eval_input: P[Ast.expr] = P( testlist ~ NEWLINE.rep ~ ENDMARKER ).map(tuplize)

  def collapse_dotted_name(name: Seq[Ast.identifier]): Ast.expr = {
    name.tail.foldLeft[Ast.expr](Ast.expr.Name(name.head, Ast.expr_context.Load))(
      (x, y) => Ast.expr.Attribute(x, y, Ast.expr_context.Load)
    )
  }

  /// decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
  val decorator: P[Ast.expr] = P( "@" ~/ dotted_name ~ ("(" ~ arglist ~ ")" ).?  ~~ Lexical.nonewlinewscomment.? ~~ NEWLINE).map{
    case (name, None) => collapse_dotted_name(name)
    case (name, Some((args, (keywords, starargs, kwargs)))) =>
      val x = collapse_dotted_name(name)
      Ast.expr.Call(x, args, keywords, starargs, kwargs)
  }

  /// decorators: decorator+
  val decorators = P( decorator.rep )

  /// decorated: decorators (classdef | funcdef | async_funcdef)
  val decorated: P[Ast.stmt] = P( decorators ~ (classdef | funcdef | async_funcdef) ).map{case (a, b) => b(a)}

  /// classdef: 'class' NAME ['(' [arglist] ')'] ':' suite
  val classdef: P[Seq[Ast.expr] => Ast.stmt.ClassDef] =
    P( kw("class") ~/ NAME ~ ("(" ~ testlist.? ~ ")").?.map(_.toSeq.flatten.flatten) ~ ":" ~~ suite ).map{
      case (a, b, c) => Ast.stmt.ClassDef(a, b, c, _)
    }

  /// async_funcdef: ASYNC funcdef
  val async_funcdef: P[Seq[Ast.expr] => Ast.stmt] = P( kw("async") ~ funcdef ).map(f => a => Ast.stmt.AsyncFunctionDef(f(a)))

  /// funcdef: 'def' NAME parameters ['->' test] ':' suite
  val funcdef: P[Seq[Ast.expr] => Ast.stmt.FunctionDef] = P( kw("def") ~/ NAME ~ parameters ~ ":" ~~ suite ).map{
    case (name, args, suite) => Ast.stmt.FunctionDef(name, args, suite, _)
  }

  /// parameters: '(' [typedargslist] ')'
  val parameters: P[Ast.arguments] = P( "(" ~ typedargslist.? ~ ")" ).map {
    _.getOrElse(Ast.arguments.empty)
  }

  /// stmt: simple_stmt | compound_stmt
  val stmt: P[Seq[Ast.stmt]] = P( compound_stmt.map(Seq(_)) | simple_stmt )

  /// simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
  val simple_stmt: P[Seq[Ast.stmt]] = P( small_stmt.rep(1, sep = ";") ~ ";".? )

  /// small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
  /// import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
  val small_stmt: P[Ast.stmt] = P(
    del_stmt | pass_stmt | flow_stmt |
    import_stmt | global_stmt | nonlocal_stmt | assert_stmt | expr_stmt
  )

  /// expr_stmt: testlist_star_expr (annassign | augassign (yield_expr|testlist) |
  ///  ('=' (yield_expr|testlist_star_expr))*)
  val expr_stmt: P[Ast.stmt] = {
    val ann = P( testlist_star_expr ~ annassign )
    val aug = P( testlist_star_expr ~ augassign ~ (yield_expr | testlist.map(tuplize)) )
    val assign = P( testlist_star_expr ~ ("=" ~ (yield_expr | testlist_star_expr.map(tuplize))).rep )

    P(
      ann.map{case (a, b) => Ast.stmt.Assign(tuplize(a), None, b) } | // todo
      aug.map{case (a, b, c) => Ast.stmt.AugAssign(tuplize(a), b, c) } |
      assign.map{
        case (a, Nil) => Ast.stmt.Expr(tuplize(a))
        case (a, b) => Ast.stmt.Assign(Seq(tuplize(a)) ++ b.init, b.last)
      }
    )
  }

  /// annassign: ':' test ['=' test]
  val annassign: P[Ast.stmt] = P( ":" ~ test ~ ("=" ~ test).?).map {
    case (ann, assign) => ??? // todo
  }

  /// augassign: ('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
  ///  '<<=' | '>>=' | '**=' | '//=')
  val augassign: P[Ast.operator] = P(
    "+=".!.map(_ => Ast.operator.Add) |
    "-=".!.map(_ => Ast.operator.Sub) |
    "*=".!.map(_ => Ast.operator.Mult) |
    "@=".!.map(_ => Ast.operator.MatrixMult) |
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

  /// del_stmt: 'del' exprlist
  val del_stmt = P( kw("del") ~~ " ".rep ~~ exprlist ).map(Ast.stmt.Delete)

  /// pass_stmt: 'pass'
  val pass_stmt = P( kw("pass") ).map(_ => Ast.stmt.Pass)

  /// flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
  val flow_stmt: P[Ast.stmt] = P( break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt )

  /// break_stmt: 'break'
  val break_stmt = P( kw("break") ).map(_ => Ast.stmt.Break)

  /// continue_stmt: 'continue'
  val continue_stmt = P( kw("continue") ).map(_ => Ast.stmt.Continue)

  /// return_stmt: 'return' [testlist]
  val return_stmt = P( kw("return") ~~ " ".rep ~~ testlist.map(tuplize).? ).map(Ast.stmt.Return)

  /// yield_stmt: yield_expr
  val yield_stmt = P( yield_expr ).map(Ast.stmt.Expr)

  /// raise_stmt: 'raise' [test ['from' test]]
  // todo
  val raise_stmt: P[Ast.stmt.Raise] = P( kw("raise") ~~ " ".rep ~~ (test ~ ("from" ~ test).?).? ).map(Ast.stmt.Raise.tupled)

  /// import_stmt: import_name | import_from
  val import_stmt: P[Ast.stmt] = P( import_name | import_from )

  /// import_name: 'import' dotted_as_names
  val import_name: P[Ast.stmt.Import] = P( kw("import") ~ dotted_as_names ).map(Ast.stmt.Import)

  /// import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
  ///   'import' ('*' | '(' import_as_names ')' | import_as_names))
  val import_from: P[Ast.stmt.ImportFrom] = {
    val named = P( ".".rep(1).!.? ~ dotted_name.!.map(Some(_)) )
    val unNamed = P( ".".rep(1).!.map(x => (Some(x), None)) )
    val star = P( "*".!.map(_ => Seq(Ast.alias(Ast.identifier("*"), None))) )
    P( kw("from") ~ (named | unNamed) ~ kw("import") ~ (star | "(" ~ import_as_names ~ ")" | import_as_names) ).map{
      case (dots, module, names) => Ast.stmt.ImportFrom(module.map(Ast.identifier), names, dots.map(_.length))
    }
  }

  /// import_as_name: NAME ['as' NAME]
  val import_as_name: P[Ast.alias] = P( NAME ~ (kw("as") ~ NAME).? ).map(Ast.alias.tupled)

  /// dotted_as_name: dotted_name ['as' NAME]
  val dotted_as_name: P[Ast.alias] = P( dotted_name.map(x => Ast.identifier(x.map(_.name).mkString("."))) ~ (kw("as") ~ NAME).? ).map(Ast.alias.tupled)

  /// import_as_names: import_as_name (',' import_as_name)* [',']
  val import_as_names = P( import_as_name.rep(1, ",") ~ (",").? )

  /// dotted_as_names: dotted_as_name (',' dotted_as_name)*
  val dotted_as_names = P( dotted_as_name.rep(1, ",") )

  /// dotted_name: NAME ('.' NAME)*
  val dotted_name = P( NAME.rep(1, ".") )

  /// global_stmt: 'global' NAME (',' NAME)*
  val global_stmt: P[Ast.stmt.Global] = P( kw("global") ~ NAME.rep(1, sep = ",") ).map(Ast.stmt.Global)

  /// nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
  val nonlocal_stmt: P[Ast.stmt.NonLocal] = P( kw("nonlocal") ~ NAME.rep(1, sep = ",") ).map(Ast.stmt.NonLocal)

  /// nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
  val assert_stmt: P[Ast.stmt.Assert] = P( kw("assert") ~ test ~ ("," ~ test).? ).map(Ast.stmt.Assert.tupled)

  /// compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt
  val compound_stmt: P[Ast.stmt] = P( if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | decorated | async_stmt)

  /// async_stmt: ASYNC (funcdef | with_stmt | for_stmt)
  private val funcdef_stmt = P( funcdef ).map(f => f(Seq()))
  val async_stmt: P[Ast.stmt] = P( kw("async") ~ (funcdef_stmt | with_stmt | for_stmt) ).map(Ast.stmt.Async)

  /// if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
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

  private val space_indents = P( spaces.repX ~~ " ".repX(indent) )

  /// while_stmt: 'while' test ':' suite ['else' ':' suite]
  val while_stmt = P( kw("while") ~/ test ~ ":" ~~ suite ~~ (space_indents ~~ kw("else") ~/ ":" ~~ suite).?.map(_.toSeq.flatten) ).map(Ast.stmt.While.tupled)

  /// for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
  val for_stmt: P[Ast.stmt.For] = P( kw("for") ~/ exprlist ~ kw("in") ~ testlist ~ ":" ~~ suite ~~ (space_indents ~ kw("else") ~/ ":" ~~ suite).? ).map {
    case (itervars, generator, body, orelse) =>
      Ast.stmt.For(tuplize(itervars), tuplize(generator), body, orelse.toSeq.flatten)
  }

  /// try_stmt: ('try' ':' suite
  ///   ((except_clause ':' suite)+
  ///    ['else' ':' suite]
  ///    ['finally' ':' suite] |
  ///   'finally' ':' suite))
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

  /// with_stmt: 'with' with_item (',' with_item)*  ':' suite
  val with_stmt: P[Ast.stmt.With] = P( kw("with") ~/ with_item.rep(1, ",")~ ":" ~~ suite ).map{
    case (items, body) =>
      val (last_expr, last_vars) = items.last
      val inner = Ast.stmt.With(last_expr, last_vars, body)
      items.init.foldRight(inner){
        case ((expr, vars), body) => Ast.stmt.With(expr, vars, Seq(body))
      }
  }

  /// with_item: test ['as' expr]
  val with_item: P[(Ast.expr, Option[Ast.expr])] = P( test ~ (kw("as") ~ expr).? )

  /// except_clause: 'except' [test ['as' NAME]]
  // NB compile.c makes sure that the default except clause is last
  val except_clause = P( space_indents ~ kw("except") ~/ (test ~ (kw("as") ~ test).?).? )

  /// suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
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
