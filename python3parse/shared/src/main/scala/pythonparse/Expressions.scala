package pythonparse

import fastparse.core.Implicits.Sequencer
import fastparse.noApi._
import WsApi._
import acyclic.file
import Lexical.kw
/**
 * Python's expression grammar. This is stuff that can be used within a larger
 * expression. Everything here ignores whitespace and does not care about
 * indentation
 *
 * Manually transcribed from https://docs.python.org/2/reference/grammar.html
 */
object Expressions {

  def tuplize(xs: Seq[Ast.expr]) = xs match{
    case Seq(x) => x
    case xs => Ast.expr.Tuple(xs, Ast.expr_context.Load)
  }

  val NAME: P[Ast.identifier] = Lexical.identifier
  val NUMBER: P[Ast.expr.Num] = P( Lexical.floatnumber | Lexical.longinteger | Lexical.integer | Lexical.imagnumber ).map(Ast.expr.Num)
  val STRING: P[Ast.string] = Lexical.stringliteral

  /// test: or_test ['if' or_test 'else' test] | lambdef
  val test: P[Ast.expr] = {
    val ternary = P( or_test ~ (kw("if") ~ or_test ~ kw("else") ~ test).? ).map{
      case (x, None) => x
      case (x, Some((test, neg))) => Ast.expr.IfExp(test, x, neg)
    }
    P( ternary | lambdef )
  }

  /// test_nocond: or_test | lambdef_nocond
  val test_nocond: P[Ast.expr] = P( or_test | lambdef_nocond )

  /// lambdef: 'lambda' [varargslist] ':' test
  val lambdef: P[Ast.expr.Lambda] = P( kw("lambda") ~ varargslist ~ ":" ~ test ).map(Ast.expr.Lambda.tupled)

  /// lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
  val lambdef_nocond: P[Ast.expr.Lambda] = P( kw("lambda") ~ varargslist ~ ":" ~ test_nocond ).map(Ast.expr.Lambda.tupled)

  /// and_test ('or' and_test)*
  val or_test = P( and_test.rep(1, kw("or")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.Or, xs)
  }

  /// not_test ('and' not_test)*
  val and_test = P( not_test.rep(1, kw("and")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.And, xs)
  }

  /// not_test: 'not' not_test | comparison
  val not_test: P[Ast.expr] = P( ("not" ~ not_test).map(Ast.expr.UnaryOp(Ast.unaryop.Not, _)) | comparison )

  /// comparison: expr (comp_op expr)*
  val comparison: P[Ast.expr] = P( expr ~ (comp_op ~ expr).rep ).map{
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.expr.Compare(lhs, ops, vals)
  }

  // Common operators, mapped from their
  // strings to their type-safe representations
  def op[T](s: P0, rhs: T) = s.!.map(_ => rhs)
  val LShift = op("<<", Ast.operator.LShift)
  val RShift = op(">>", Ast.operator.RShift)
  val Lt = op("<", Ast.cmpop.Lt)
  val Gt = op(">", Ast.cmpop.Gt)
  val Eq = op("==", Ast.cmpop.Eq)
  val GtE = op(">=", Ast.cmpop.GtE)
  val LtE = op("<=", Ast.cmpop.LtE)
  //# <> isn't actually a valid comparison operator in Python. It's here for the
  //# sake of a __future__ import described in PEP 401 (which really works :-)
  val NotEq = op("<>" | "!=", Ast.cmpop.NotEq)
  val In = op("in", Ast.cmpop.In)
  val NotIn = op("not" ~ "in", Ast.cmpop.NotIn)
  val Is = op("is", Ast.cmpop.Is)
  val IsNot = op("is" ~ "not", Ast.cmpop.IsNot)

  /// comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
  val comp_op = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )

  /// star_expr: '*' expr
  val star_expr: P[Ast.expr] = P( "*" ~ expr.map(Ast.expr.StarExpr) )

  private val starstar_expr: P[Ast.expr] = P( "**" ~ expr.map(Ast.expr.StarStarExpr) )

  val Add = op("+", Ast.operator.Add)
  val Sub = op("-", Ast.operator.Sub)
  val Pow = op("**", Ast.operator.Pow)
  val Mult= op("*", Ast.operator.Mult)
  val MatrixMult = op("@", Ast.operator.MatrixMult)
  val Div = op("/", Ast.operator.Div)
  val Mod = op("%", Ast.operator.Mod)
  val FloorDiv = op("//", Ast.operator.FloorDiv)
  val BitOr = op("|", Ast.operator.BitOr)
  val BitAnd = op("&", Ast.operator.BitAnd)
  val BitXor = op("^", Ast.operator.BitXor)
  val UAdd = op("+", Ast.unaryop.UAdd)
  val USub = op("-", Ast.unaryop.USub)
  val Invert = op("~", Ast.unaryop.Invert)
  val unary_op = P ( UAdd | USub | Invert )


  def Unary(p: P[Ast.expr]) =
    (unary_op ~ p).map{ case (op, operand) => Ast.expr.UnaryOp(op, operand) }

  def Chain(p: P[Ast.expr], op: P[Ast.operator]) = P( p ~ (op ~ p).rep ).map{
    case (lhs, chunks) =>
      chunks.foldLeft(lhs){case (lhs, (op, rhs)) =>
        Ast.expr.BinOp(lhs, op, rhs)
      }
  }

  /// expr: xor_expr ('|' xor_expr)*
  val expr: P[Ast.expr] = P( Chain(xor_expr, BitOr) )

  /// xor_expr: and_expr ('^' and_expr)*
  val xor_expr: P[Ast.expr] = P( Chain(and_expr, BitXor) )

  /// and_expr: shift_expr ('&' shift_expr)*
  val and_expr: P[Ast.expr] = P( Chain(shift_expr, BitAnd) )

  /// shift_expr: arith_expr (('<<'|'>>') arith_expr)*
  val shift_expr: P[Ast.expr] = P( Chain(arith_expr, LShift | RShift) )

  /// arith_expr: term (('+'|'-') term)*
  val arith_expr: P[Ast.expr] = P( Chain(term, Add | Sub) )

  /// term: factor (('*'|'@'|'/'|'%'|'//') factor)*
  val term: P[Ast.expr] = P( Chain(factor, Mult | MatrixMult | Div | Mod | FloorDiv) )

  /// factor: ('+'|'-'|'~') factor | power
  // NUMBER appears here and below in `atom` to give it precedence.
  // This ensures that "-2" will parse as `Num(-2)` rather than
  // as `UnaryOp(USub, Num(2))`.
  val factor: P[Ast.expr] = P( NUMBER | Unary(factor) | power )

  /// power: atom_expr ['**' factor]
  val power: P[Ast.expr] = P( atom_expr ~ trailer.rep ~ (Pow ~ factor).? ).map{
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.expr.BinOp(left, op, right)
      }
  }

  /// atom_expr: [AWAIT] atom trailer*
  val atom_expr: P[Ast.expr] = P( kw("await") ~ atom).map(Ast.expr.Await)

  /// atom: ('(' [yield_expr|testlist_comp] ')' |
  ///        '[' [testlist_comp] ']' |
  ///        '{' [dictorsetmaker] '}' |
  ///        NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
  val atom: P[Ast.expr] = {
    val empty_tuple = ("(" ~ ")").map(_ => Ast.expr.Tuple(Nil, Ast.expr_context.Load))
    val empty_list = ("[" ~ "]").map(_ => Ast.expr.List(Nil, Ast.expr_context.Load))
    val empty_dict = ("{" ~ "}").map(_ => Ast.expr.Dict(Seq()))
    P(
      empty_tuple  |
      empty_list |
      empty_dict |
      "(" ~ (yield_expr | testlist_comp) ~ ")" |
      "[" ~ testlist_comp ~ "]" |
      "{" ~ dictorsetmaker ~ "}" |
      STRING.rep(1).map(_.mkString).map(Ast.expr.Str) |
      NAME.map(Ast.expr.Name(_, Ast.expr_context.Load)) |
      NUMBER
    )
  }

  /// testlist_comp: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )
  val testlist_comp: P[Ast.expr] = {
    val test_or_star = P( test | star_expr )
    val set: P[Ast.expr.Set] = P( test_or_star.rep(1, ",") ~ ",".?).map(Ast.expr.Set)
    val set_comp = P( test_or_star ~ comp_for ).map(Ast.expr.SetComp.tupled)
    P( set_comp | set )
  }

  /// trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
  val trailer: P[Ast.expr => Ast.expr] = {
    val call = P("(" ~ arglist ~ ")").map{ case (args, (keywords, starargs, kwargs)) => (lhs: Ast.expr) => Ast.expr.Call(lhs, args, keywords, starargs, kwargs)}
    val slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.expr) => Ast.expr.Subscript(lhs, args, Ast.expr_context.Load))
    val attr = P("." ~ NAME).map(id => (lhs: Ast.expr) => Ast.expr.Attribute(lhs, id, Ast.expr_context.Load))
    P( call | slice | attr )
  }

  /// subscriptlist: subscript (',' subscript)* [',']
  val subscriptlist = P( subscript.rep(1, ",") ~ ",".? ).map{
    case Seq(x) => x
    case xs => Ast.slice.ExtSlice(xs)
  }

  /// subscript: test | [test] ':' [test] [sliceop]
  val subscript: P[Ast.slice] = {
    val single = P( test.map(Ast.slice.Index) )
    val multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      Ast.slice.Slice(
        lower,
        upper,
        step.map(_.getOrElse(Ast.expr.Name(Ast.identifier("None"), Ast.expr_context.Load)))
      )
    }
    P( multi | single )
  }

  /// sliceop: ':' [test]
  val sliceop = P( ":" ~ test.? )

  /// exprlist: (expr|star_expr) (',' (expr|star_expr))* [',']
  val exprlist: P[Seq[Ast.expr]] = P( (expr | star_expr).rep(1, sep = ",") ~ ",".? )

  /// testlist: test (',' test)* [',']
  val testlist: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") ~ ",".? ).map(Ast.testlist.tupled)

  /// testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
  val testlist_star_expr: P[Seq[Ast.expr]] = P( (test|star_expr).rep(1, sep = ",") ~ ",".? )

  /// dictorsetmaker: ( ((test ':' test | '**' expr)
  ///                    (comp_for | (',' (test ':' test | '**' expr))* [','])) |
  ///                   ((test | star_expr)
  ///                    (comp_for | (',' (test | star_expr))* [','])) )
  val dictorsetmaker: P[Ast.expr] = {
    val key_value = P( test ~ ":" ~ test ).map(Ast.expr.KeyValueExpr)
    val dict_item: P[Ast.expr.DictItem] = P( key_value | starstar_expr )
    val dict: P[Ast.expr.Dict] = P( dict_item.rep(1, ",") ~ ",".? ).map(Ast.expr.Dict.tupled)
    val dict_comp = P(
      (dict_item ~ comp_for).map(Ast.expr.DictComp.tupled)
    )
    P( dict_comp | dict | testlist_comp)
  }

  /// arglist: argument (',' argument)*  [',']
  val arglist = P( argument.rep(1, ",") ~ ",".? )

  /// argument: ( test [comp_for] |
  ///             test '=' test |  # meant: NAME '=' test
  ///             '**' test |
  ///             '*' test )
  val argument = {
    val arg_comp: P[Ast.expr] = P( test ~ comp_for.? ).map {
      case (x, None) => x
      case (x, Some(gen)) => Ast.expr.GeneratorExp(x, gen)
    }
    val arg_named = P( NAME ~ "=" ~ test  ).map(Ast.keyword.tupled)
    P( arg_comp | arg_named | star_expr | starstar_expr )
  }

  /// comp_iter: comp_for | comp_if
  val comp_iter = P( comp_for | comp_if )

  /// comp_for: [ASYNC] 'for' exprlist 'in' or_test [comp_iter]
  val comp_for: P[Ast.comprehension] = P( "async".!.? ~ "for" ~ exprlist ~ "in" ~ or_test ~ comp_iter.? ).map{
    case (async, targets, test, iter) => Ast.comprehension(tuplize(targets), test, iter, async = async.isDefined)
  }

  /// comp_if: 'if' test_nocond [comp_iter]
  val comp_if: P[Ast.expr] = P( "if" ~ test_nocond ~ comp_iter.? ).map {
    case (test, iter) => Ast.ifExpr(test, iter)
  }

  /// # not used in grammar, but may appear in "node" passed from Parser to Compiler
  //val encoding_decl = P( NAME )

  /// yield_expr: 'yield' [yield_arg]
  val yield_expr: P[Ast.expr.Yield] = P( kw("yield") ~ yield_arg.? ).map(Ast.expr.Yield)

  /// yield_arg: 'from' test | testlist
  val yield_arg: P[Ast.expr] = P( "from" ~ test | testlist )

  /// typedargslist: (tfpdef ['=' test] (',' tfpdef ['=' test])* [',' [
  ///             '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
  ///          | '**' tfpdef [',']]]
  ///   | '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
  ///   | '**' tfpdef [','])
  val typedargslist: P[Ast.arguments] = {
    type NamedArg = (Ast.expr.Name, Option[Ast.expr])
    val named_arg: P[NamedArg] = P( tfpdef ~ ("=" ~ test).? )
    type StarArgs = (Option[Ast.expr.Name], Seq[NamedArg], Option[Ast.expr.Name])
    val star_args: P[StarArgs]
      = P( ("*" ~ tfpdef.? ~ ("," ~ named_arg.repX(sep = ",")).? ~ ("," ~ "**" ~ tfpdef).?) | "**" ~ tfpdef ).map {
      case (vararg: Option[Ast.expr.Name], nargs: Option[Seq[NamedArg]], kwarg: Option[Ast.expr.Name]) =>
        (vararg, nargs.getOrElse(Seq()), kwarg)
      case kwarg: Ast.expr.Name =>
        (None, Seq(), Some(kwarg))
    }
    val x = P( (named_arg.repX(sep = ",") ~ ("," ~ star_args).?) | star_args ).map {
      case (normal: Seq[NamedArg], star: Option[StarArgs]) =>
        val (args, defaults) = normal.unzip
        val (vararg, nargs, kwarg) = star.getOrElse((None, Seq(), None))
        val (namedArgs, namedDefaults) = nargs.unzip
        Ast.arguments(args, vararg, namedArgs, kwarg, defaults.flatten, namedDefaults.flatten)
      case star: StarArgs =>
        val (vararg, nargs, kwarg) = star
        val (namedArgs, namedDefaults) = nargs.unzip
        Ast.arguments(Seq(), vararg, namedArgs, kwarg, Seq(), namedDefaults.flatten)
    }
    P( x )
  }

  /// tfpdef: NAME [':' test]
  val tfpdef: P[Ast.expr.Name] = P( NAME ~ (":" ~ test).?).map {
    case (name, annot) =>
      Ast.expr.Name(name, Ast.expr_context.Param, annot)
  }

  /// varargslist: (vfpdef ['=' test] (',' vfpdef ['=' test])* [',' [
  ///          '*' [vfpdef] (',' vfpdef ['=' test])* [',' ['**' vfpdef [',']]]
  ///       | '**' vfpdef [',']]]
  ///   | '*' [vfpdef] (',' vfpdef ['=' test])* [',' ['**' vfpdef [',']]]
  ///   | '**' vfpdef [',']
  /// )
  val varargslist: P[Ast.arguments] = {
    type NamedArg = Ast.expr.Name
    val named_arg: P[NamedArg] = P( vfpdef ~ ("=" ~ test).? )
    type StarArgs = (Option[Ast.expr.Name], Seq[NamedArg], Option[Ast.expr.Name])
    val star_args: P[StarArgs]
    = P( ("*" ~ vfpdef.? ~ ("," ~ named_arg.repX(sep = ",")).? ~ ("," ~ "**" ~ vfpdef).?) | "**" ~ vfpdef ).map {
      case (vararg: Option[Ast.expr.Name], nargs: Option[Seq[NamedArg]], kwarg: Option[Ast.expr.Name]) =>
        (vararg, nargs.getOrElse(Seq()), kwarg)
      case kwarg: Ast.expr.Name =>
        (None, Seq(), Some(kwarg))
    }
    val x = P( (named_arg.repX(sep = ",") ~ ("," ~ star_args).?) | star_args ).map {
      case (normal: Seq[NamedArg], star: Option[StarArgs]) =>
        val (args, defaults) = normal.unzip
        val (vararg, nargs, kwarg) = star.getOrElse((None, Seq(), None))
        val (namedArgs, namedDefaults) = nargs.unzip
        Ast.arguments(args, vararg, namedArgs, kwarg, defaults.flatten, namedDefaults.flatten)
      case star: StarArgs =>
        val (vararg, nargs, kwarg) = star
        val (namedArgs, namedDefaults) = nargs.unzip
        Ast.arguments(Seq(), vararg, namedArgs, kwarg, Seq(), namedDefaults.flatten)
    }
    P( x )
  }

  /// vfpdef: NAME
  val vfpdef: P[Ast.expr.Name] = P( NAME ).map { name =>
    Ast.expr.Name(name, Ast.expr_context.Param, None)
  }
}
