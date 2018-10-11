package pythonparse

import fasterparser._

import Lexical.kw
/**
 * Python's expression grammar. This is stuff that can be used within a larger
 * expression. Everything here ignores whitespace and does not care about
 * indentation
 *
 * Manually transcribed from https://docs.python.org/2/reference/grammar.html
 */
object Expressions {
  implicit def whitespace(cfg: P[_]): P[Unit] = Lexical.wscomment(cfg)
  def tuplize(xs: Seq[Ast.expr]) = xs match{
    case Seq(x) => x
    case xs => Ast.expr.Tuple(xs, Ast.expr_context.Load)
  }

  def NAME[_: P]: P[Ast.identifier] = Lexical.identifier
  def NUMBER[_: P]: P[Ast.expr.Num] = P( Lexical.floatnumber | Lexical.longinteger | Lexical.integer | Lexical.imagnumber ).map(Ast.expr.Num)
  def STRING[_: P]: P[Ast.string] = Lexical.stringliteral

  def test[_: P]: P[Ast.expr] = {
    def ternary = P( or_test ~ (kw("if") ~ or_test ~ kw("else") ~ test).? ).map{
      case (x, None) => x
      case (x, Some((test, neg))) => Ast.expr.IfExp(test, x, neg)
    }
    P( ternary | lambdef )
  }
  def or_test[_: P] = P( and_test.rep(1, sep = kw("or")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.Or, xs)
  }
  def and_test[_: P] = P( not_test.rep(1, sep = kw("and")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.And, xs)
  }
  def not_test[_: P]: P[Ast.expr] = P( ("not" ~ not_test).map(Ast.expr.UnaryOp(Ast.unaryop.Not, _)) | comparison )
  def comparison[_: P]: P[Ast.expr] = P( expr ~ (comp_op ~ expr).rep ).map{
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.expr.Compare(lhs, ops, vals)
  }

  // Common operators, mapped from their
  // strings to their type-safe representations
  def op[T, _: P](s: => P[Unit], rhs: T) = s.!.map(_ => rhs)
  def LShift[_: P] = op("<<", Ast.operator.LShift)
  def RShift[_: P] = op(">>", Ast.operator.RShift)
  def Lt[_: P] = op("<", Ast.cmpop.Lt)
  def Gt[_: P] = op(">", Ast.cmpop.Gt)
  def Eq[_: P] = op("==", Ast.cmpop.Eq)
  def GtE[_: P] = op(">=", Ast.cmpop.GtE)
  def LtE[_: P] = op("<=", Ast.cmpop.LtE)
  def NotEq[_: P] = op("<>" | "!=", Ast.cmpop.NotEq)
  def In[_: P] = op("in", Ast.cmpop.In)
  def NotIn[_: P] = op("not" ~ "in", Ast.cmpop.NotIn)
  def Is[_: P] = op("is", Ast.cmpop.Is)
  def IsNot[_: P] = op("is" ~ "not", Ast.cmpop.IsNot)
  def comp_op[_: P] = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )
  def Add[_: P] = op("+", Ast.operator.Add)
  def Sub[_: P] = op("-", Ast.operator.Sub)
  def Pow[_: P] = op("**", Ast.operator.Pow)
  def Mult[_: P] = op("*", Ast.operator.Mult)
  def Div[_: P] = op("/", Ast.operator.Div)
  def Mod[_: P] = op("%", Ast.operator.Mod)
  def FloorDiv[_: P] = op("//", Ast.operator.FloorDiv)
  def BitOr[_: P] = op("|", Ast.operator.BitOr)
  def BitAnd[_: P] = op("&", Ast.operator.BitAnd)
  def BitXor[_: P] = op("^", Ast.operator.BitXor)
  def UAdd[_: P] = op("+", Ast.unaryop.UAdd)
  def USub[_: P] = op("-", Ast.unaryop.USub)
  def Invert[_: P] = op("~", Ast.unaryop.Invert)
  def unary_op[_: P] = P ( UAdd | USub | Invert )


  def Unary[_: P](p: => P[Ast.expr]) =
    (unary_op ~ p).map{ case (op, operand) => Ast.expr.UnaryOp(op, operand) }

  def Chain[_: P](p: => P[Ast.expr], op: => P[Ast.operator]) = P( p ~ (op ~ p).rep ).map{
    case (lhs, chunks) =>
      chunks.foldLeft(lhs){case (lhs, (op, rhs)) =>
        Ast.expr.BinOp(lhs, op, rhs)
      }
  }
  def expr[_: P]: P[Ast.expr] = P( Chain(xor_expr, BitOr) )
  def xor_expr[_: P]: P[Ast.expr] = P( Chain(and_expr, BitXor) )
  def and_expr[_: P]: P[Ast.expr] = P( Chain(shift_expr, BitAnd) )
  def shift_expr[_: P]: P[Ast.expr] = P( Chain(arith_expr, LShift | RShift) )

  def arith_expr[_: P]: P[Ast.expr] = P( Chain(term, Add | Sub) )
  def term[_: P]: P[Ast.expr] = P( Chain(factor, Mult | FloorDiv | Div | Mod ) )

  def factor[_: P]: P[Ast.expr] = P( power | Unary(factor) )
  def power[_: P]: P[Ast.expr] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map{
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.expr.BinOp(left, op, right)
      }
  }
  def atom[_: P]: P[Ast.expr] = {
    def empty_tuple = ("(" ~ ")").map(_ => Ast.expr.Tuple(Nil, Ast.expr_context.Load))
    def empty_list = ("[" ~ "]").map(_ => Ast.expr.List(Nil, Ast.expr_context.Load))
    def empty_dict = ("{" ~ "}").map(_ => Ast.expr.Dict(Nil, Nil))
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
  def list_contents[_: P] = P( test.rep(1, ",") ~ ",".? )
  def list[_: P] = P( list_contents ).map(Ast.expr.List(_, Ast.expr_context.Load))
  def tuple_contents[_: P] = P( test ~ "," ~ list_contents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  def tuple[_: P] = P( tuple_contents).map(Ast.expr.Tuple(_, Ast.expr_context.Load))
  def list_comp_contents[_: P] = P( test ~ comp_for.rep(1) )
  def list_comp[_: P] = P( list_comp_contents ).map(Ast.expr.ListComp.tupled)
  def generator[_: P] = P( list_comp_contents ).map(Ast.expr.GeneratorExp.tupled)

  def lambdef[_: P]: P[Ast.expr.Lambda] = P( kw("lambda") ~ varargslist ~ ":" ~ test ).map(Ast.expr.Lambda.tupled)
  def trailer[_: P]: P[Ast.expr => Ast.expr] = {
    def call = P("(" ~ arglist ~ ")").map{ case (args, (keywords, starargs, kwargs)) => (lhs: Ast.expr) => Ast.expr.Call(lhs, args, keywords, starargs, kwargs)}
    def slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.expr) => Ast.expr.Subscript(lhs, args, Ast.expr_context.Load))
    def attr = P("." ~ NAME).map(id => (lhs: Ast.expr) => Ast.expr.Attribute(lhs, id, Ast.expr_context.Load))
    P( call | slice | attr )
  }
  def subscriptlist[_: P] = P( subscript.rep(1, ",") ~ ",".? ).map{
    case Seq(x) => x
    case xs => Ast.slice.ExtSlice(xs)
  }
  def subscript[_: P]: P[Ast.slice] = {
    def ellipses = P( ("." ~ "." ~ ".").map(_ => Ast.slice.Ellipsis) )
    def single = P( test.map(Ast.slice.Index) )
    def multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      Ast.slice.Slice(
        lower,
        upper,
        step.map(_.getOrElse(Ast.expr.Name(Ast.identifier("None"), Ast.expr_context.Load)))
      )
    }
    P( ellipses | multi | single )
  }

  def sliceop[_: P] = P( ":" ~ test.? )
  def exprlist[_: P]: P[Seq[Ast.expr]] = P( expr.rep(1, sep = ",") ~ ",".? )
  def testlist[_: P]: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") ~ ",".? )
  def dictorsetmaker[_: P]: P[Ast.expr] = {
    def dict_item = P( test ~ ":" ~ test )
    def dict: P[Ast.expr.Dict] = P(
      (dict_item.rep(1, ",") ~ ",".?).map{x =>
        val (keys, values) = x.unzip
        Ast.expr.Dict(keys, values)
      }
    )
    def dict_comp = P(
      (dict_item ~ comp_for.rep(1)).map(Ast.expr.DictComp.tupled)
    )
    def set: P[Ast.expr.Set] = P( test.rep(1, ",") ~ ",".? ).map(Ast.expr.Set)
    def set_comp = P( test ~ comp_for.rep(1) ).map(Ast.expr.SetComp.tupled)
    P( dict_comp | dict | set_comp | set)
  }

  def arglist[_: P] = {
    def inits = P( (plain_argument ~ !"=").rep(0, ",") )
    def later = P( named_argument.rep(0, ",") ~ ",".? ~ ("*" ~ test).? ~ ",".? ~ ("**" ~ test).? ~ ",".? ~ named_argument.rep(0, ",")).map{
      case (named1, dot, star, named2) => (named1 ++ named2, dot, star )
    }
    P( inits ~ ",".? ~ later )
  }

  def plain_argument[_: P] = P( test ~ comp_for.rep ).map{
    case (x, Nil) => x
    case (x, gens) => Ast.expr.GeneratorExp(x, gens)
  }
  def named_argument[_: P] = P( NAME ~ "=" ~ test  ).map(Ast.keyword.tupled)

  def comp_for[_: P]: P[Ast.comprehension] = P( "for" ~ exprlist ~ "in" ~ or_test ~ comp_if.rep ).map{
    case (targets, test, ifs) => Ast.comprehension(tuplize(targets), test, ifs)
  }
  def comp_if[_: P]: P[Ast.expr] = P( "if" ~ test )

  def testlist1[_: P]: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") )

  // not used in grammar, but may appear in "node" passed from Parser to Compiler
  //  def encoding_decl[_: P]: P0 = P( NAME )

  def yield_expr[_: P]: P[Ast.expr.Yield] = P( kw("yield") ~ testlist.map(tuplize).? ).map(Ast.expr.Yield)

  def varargslist[_: P]: P[Ast.arguments] = {
    def named_arg = P( fpdef ~ ("=" ~ test).? )
    def x = P( named_arg.rep(sep = ",") ~ ",".? ~ ("*" ~ NAME).? ~ ",".? ~ ("**" ~ NAME).? ).map{
      case (normal_args, starargs, kwargs) =>
        val (args, defaults) = normal_args.unzip
        Ast.arguments(args, starargs, kwargs, defaults.flatten)
    }
    P( x )
  }

  def fpdef[_: P]: P[Ast.expr] = P( NAME.map(Ast.expr.Name(_, Ast.expr_context.Param)) | "(" ~ fplist ~ ")" )
  def fplist[_: P]: P[Ast.expr] = P( fpdef.rep(sep = ",") ~ ",".? ).map(Ast.expr.Tuple(_, Ast.expr_context.Param))
}
