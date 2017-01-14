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

  val test: P[Ast.expr] = {
    val ternary = P( or_test ~ (kw("if") ~ or_test ~ kw("else") ~ test).? ).map{
      case (x, None) => x
      case (x, Some((test, neg))) => Ast.expr.IfExp(test, x, neg)
    }
    P( ternary | lambdef )
  }
  val or_test = P( and_test.rep(1, kw("or")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.Or, xs)
  }
  val and_test = P( not_test.rep(1, kw("and")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.And, xs)
  }
  val not_test: P[Ast.expr] = P( ("not" ~ not_test).map(Ast.expr.UnaryOp(Ast.unaryop.Not, _)) | comparison )
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
  val NotEq = op("<>" | "!=", Ast.cmpop.NotEq)
  val In = op("in", Ast.cmpop.In)
  val NotIn = op("not" ~ "in", Ast.cmpop.NotIn)
  val Is = op("is", Ast.cmpop.Is)
  val IsNot = op("is" ~ "not", Ast.cmpop.IsNot)
  val comp_op = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )
  val Add = op("+", Ast.operator.Add)
  val Sub = op("-", Ast.operator.Sub)
  val Pow = op("**", Ast.operator.Pow)
  val Mult= op("*", Ast.operator.Mult)
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
  val expr: P[Ast.expr] = P( Chain(xor_expr, BitOr) )
  val xor_expr: P[Ast.expr] = P( Chain(and_expr, BitXor) )
  val and_expr: P[Ast.expr] = P( Chain(shift_expr, BitAnd) )
  val shift_expr: P[Ast.expr] = P( Chain(arith_expr, LShift | RShift) )

  val arith_expr: P[Ast.expr] = P( Chain(term, Add | Sub) )
  val term: P[Ast.expr] = P( Chain(factor, Mult | Div | Mod | FloorDiv) )
  // NUMBER appears here and below in `atom` to give it precedence.
  // This ensures that "-2" will parse as `Num(-2)` rather than
  // as `UnaryOp(USub, Num(2))`.
  val factor: P[Ast.expr] = P( NUMBER | Unary(factor) | power )
  val power: P[Ast.expr] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map{
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.expr.BinOp(left, op, right)
      }
  }
  val atom: P[Ast.expr] = {
    val empty_tuple = ("(" ~ ")").map(_ => Ast.expr.Tuple(Nil, Ast.expr_context.Load))
    val empty_list = ("[" ~ "]").map(_ => Ast.expr.List(Nil, Ast.expr_context.Load))
    val empty_dict = ("{" ~ "}").map(_ => Ast.expr.Dict(Nil, Nil))
    P(
      empty_tuple  |
      empty_list |
      empty_dict |
      "(" ~ (yield_expr | generator | tuple | test) ~ ")" |
      "[" ~ (list_comp | list) ~ "]" |
      "{" ~ dictorsetmaker ~ "}" |
      "`" ~ testlist1.map(x => Ast.expr.Repr(Ast.expr.Tuple(x, Ast.expr_context.Load))) ~ "`" |
      STRING.rep(1).map(_.mkString).map(Ast.expr.Str) |
      NAME.map(Ast.expr.Name(_, Ast.expr_context.Load)) |
      NUMBER
    )
  }
  val list_contents = P( test.rep(1, ",") ~ ",".? )
  val list = P( list_contents ).map(Ast.expr.List(_, Ast.expr_context.Load))
  val tuple_contents = P( test ~ "," ~ list_contents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  val tuple = P( tuple_contents).map(Ast.expr.Tuple(_, Ast.expr_context.Load))
  val list_comp_contents = P( test ~ comp_for.rep(1) )
  val list_comp = P( list_comp_contents ).map(Ast.expr.ListComp.tupled)
  val generator = P( list_comp_contents ).map(Ast.expr.GeneratorExp.tupled)

  val lambdef: P[Ast.expr.Lambda] = P( kw("lambda") ~ varargslist ~ ":" ~ test ).map(Ast.expr.Lambda.tupled)
  val trailer: P[Ast.expr => Ast.expr] = {
    val call = P("(" ~ arglist ~ ")").map{ case (args, (keywords, starargs, kwargs)) => (lhs: Ast.expr) => Ast.expr.Call(lhs, args, keywords, starargs, kwargs)}
    val slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.expr) => Ast.expr.Subscript(lhs, args, Ast.expr_context.Load))
    val attr = P("." ~ NAME).map(id => (lhs: Ast.expr) => Ast.expr.Attribute(lhs, id, Ast.expr_context.Load))
    P( call | slice | attr )
  }
  val subscriptlist = P( subscript.rep(1, ",") ~ ",".? ).map{
    case Seq(x) => x
    case xs => Ast.slice.ExtSlice(xs)
  }
  val subscript: P[Ast.slice] = {
    val ellipses = P( ("." ~ "." ~ ".").map(_ => Ast.slice.Ellipsis) )
    val single = P( test.map(Ast.slice.Index) )
    val multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      Ast.slice.Slice(
        lower,
        upper,
        step.map(_.getOrElse(Ast.expr.Name(Ast.identifier("None"), Ast.expr_context.Load)))
      )
    }
    P( ellipses | multi | single )
  }

  val sliceop = P( ":" ~ test.? )
  val exprlist: P[Seq[Ast.expr]] = P( expr.rep(1, sep = ",") ~ ",".? )
  val testlist: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") ~ ",".? )
  val dictorsetmaker: P[Ast.expr] = {
    val dict_item = P( test ~ ":" ~ test )
    val dict: P[Ast.expr.Dict] = P(
      (dict_item.rep(1, ",") ~ ",".?).map{x =>
        val (keys, values) = x.unzip
        Ast.expr.Dict(keys, values)
      }
    )
    val dict_comp = P(
      (dict_item ~ comp_for.rep(1)).map(Ast.expr.DictComp.tupled)
    )
    val set: P[Ast.expr.Set] = P( test.rep(1, ",") ~ ",".? ).map(Ast.expr.Set)
    val set_comp = P( test ~ comp_for.rep(1) ).map(Ast.expr.SetComp.tupled)
    P( dict_comp | dict | set_comp | set)
  }

  val arglist = {
    val inits = P( (plain_argument ~ !"=").rep(0, ",") )
    val later = P( named_argument.rep(0, ",") ~ ",".? ~ ("*" ~ test).? ~ ",".? ~ ("**" ~ test).? )
    P( inits ~ ",".? ~ later )
  }

  val plain_argument = P( test ~ comp_for.rep ).map{
    case (x, Nil) => x
    case (x, gens) => Ast.expr.GeneratorExp(x, gens)
  }
  val named_argument = P( NAME ~ "=" ~ test  ).map(Ast.keyword.tupled)

  val comp_for: P[Ast.comprehension] = P( "for" ~ exprlist ~ "in" ~ or_test ~ comp_if.rep ).map{
    case (targets, test, ifs) => Ast.comprehension(tuplize(targets), test, ifs)
  }
  val comp_if: P[Ast.expr] = P( "if" ~ test )

  val testlist1: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") )

  // not used in grammar, but may appear in "node" passed from Parser to Compiler
  //  val encoding_decl: P0 = P( NAME )

  val yield_expr: P[Ast.expr.Yield] = P( kw("yield") ~ testlist.map(tuplize).? ).map(Ast.expr.Yield)

  val varargslist: P[Ast.arguments] = {
    val named_arg = P( fpdef ~ ("=" ~ test).? )
    val x = P( named_arg.rep(sep = ",") ~ ",".? ~ ("*" ~ NAME).? ~ ",".? ~ ("**" ~ NAME).? ).map{
      case (normal_args, starargs, kwargs) =>
        val (args, defaults) = normal_args.unzip
        Ast.arguments(args, starargs, kwargs, defaults.flatten)
    }
    P( x )
  }

  val fpdef: P[Ast.expr] = P( NAME.map(Ast.expr.Name(_, Ast.expr_context.Param)) | "(" ~ fplist ~ ")" )
  val fplist: P[Ast.expr] = P( fpdef.rep(sep = ",") ~ ",".? ).map(Ast.expr.Tuple(_, Ast.expr_context.Param))
}
