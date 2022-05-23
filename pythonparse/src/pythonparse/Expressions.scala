package pythonparse

import fastparse._

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

  def NAME[_p: P]: P[Ast.identifier] = Lexical.identifier
  def NUMBER[_p: P]: P[Ast.expr.Num] = P( Lexical.floatnumber | Lexical.longinteger | Lexical.integer | Lexical.imagnumber ).map(Ast.expr.Num.apply)
  def STRING[_p: P]: P[Ast.string] = Lexical.stringliteral

  def test[_p: P]: P[Ast.expr] = {
    def ternary = P( or_test ~ (kw("if") ~ or_test ~ kw("else") ~ test).? ).map{
      case (x, None) => x
      case (x, Some((test, neg))) => Ast.expr.IfExp(test, x, neg)
    }
    P( ternary | lambdef )
  }
  def or_test[_p: P] = P( and_test.rep(1, sep = kw("or")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.Or, xs)
  }
  def and_test[_p: P] = P( not_test.rep(1, sep = kw("and")) ).map{
    case Seq(x) => x
    case xs => Ast.expr.BoolOp(Ast.boolop.And, xs)
  }
  def not_test[_p: P]: P[Ast.expr] = P( (kw("not") ~ not_test).map(Ast.expr.UnaryOp(Ast.unaryop.Not, _)) | comparison )
  def comparison[_p: P]: P[Ast.expr] = P( expr ~ (comp_op ~ expr).rep ).map{
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.expr.Compare(lhs, ops, vals)
  }

  // Common operators, mapped from their
  // strings to their type-safe representations
  def op[T, _p: P](s: => P[Unit], rhs: T) = s.!.map(_ => rhs)
  def LShift[_p: P] = op("<<", Ast.operator.LShift)
  def RShift[_p: P] = op(">>", Ast.operator.RShift)
  def Lt[_p: P] = op("<", Ast.cmpop.Lt)
  def Gt[_p: P] = op(">", Ast.cmpop.Gt)
  def Eq[_p: P] = op("==", Ast.cmpop.Eq)
  def GtE[_p: P] = op(">=", Ast.cmpop.GtE)
  def LtE[_p: P] = op("<=", Ast.cmpop.LtE)
  def NotEq[_p: P] = op("<>" | "!=", Ast.cmpop.NotEq)
  def In[_p: P] = op(kw("in"), Ast.cmpop.In)
  def NotIn[_p: P] = op(kw("not") ~ kw("in"), Ast.cmpop.NotIn)
  def Is[_p: P] = op(kw("is"), Ast.cmpop.Is)
  def IsNot[_p: P] = op(kw("is") ~ kw("not"), Ast.cmpop.IsNot)
  def comp_op[_p: P] = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )
  def Add[_p: P] = op("+", Ast.operator.Add)
  def Sub[_p: P] = op("-", Ast.operator.Sub)
  def Pow[_p: P] = op("**", Ast.operator.Pow)
  def Mult[_p: P] = op("*", Ast.operator.Mult)
  def Div[_p: P] = op("/", Ast.operator.Div)
  def Mod[_p: P] = op("%", Ast.operator.Mod)
  def FloorDiv[_p: P] = op("//", Ast.operator.FloorDiv)
  def BitOr[_p: P] = op("|", Ast.operator.BitOr)
  def BitAnd[_p: P] = op("&", Ast.operator.BitAnd)
  def BitXor[_p: P] = op("^", Ast.operator.BitXor)
  def UAdd[_p: P] = op("+", Ast.unaryop.UAdd)
  def USub[_p: P] = op("-", Ast.unaryop.USub)
  def Invert[_p: P] = op("~", Ast.unaryop.Invert)
  def unary_op[_p: P] = P ( UAdd | USub | Invert )


  def Unary[_p: P](p: => P[Ast.expr]) =
    (unary_op ~ p).map{ case (op, operand) => Ast.expr.UnaryOp(op, operand) }

  def Chain[_p: P](p: => P[Ast.expr], op: => P[Ast.operator]) = P( p ~ (op ~ p).rep ).map{
    case (lhs, chunks) =>
      chunks.foldLeft(lhs){case (lhs, (op, rhs)) =>
        Ast.expr.BinOp(lhs, op, rhs)
      }
  }
  def expr[_p: P]: P[Ast.expr] = P( Chain(xor_expr, BitOr) )
  def xor_expr[_p: P]: P[Ast.expr] = P( Chain(and_expr, BitXor) )
  def and_expr[_p: P]: P[Ast.expr] = P( Chain(shift_expr, BitAnd) )
  def shift_expr[_p: P]: P[Ast.expr] = P( Chain(arith_expr, LShift | RShift) )

  def arith_expr[_p: P]: P[Ast.expr] = P( Chain(term, Add | Sub) )
  def term[_p: P]: P[Ast.expr] = P( Chain(factor, Mult | FloorDiv | Div | Mod ) )

  def factor[_p: P]: P[Ast.expr] = P( power | Unary(factor) )
  def power[_p: P]: P[Ast.expr] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map{
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.expr.BinOp(left, op, right)
      }
  }
  def atom[_p: P]: P[Ast.expr] = {
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
      STRING.rep(1).map(_.mkString).map(Ast.expr.Str.apply) |
      NAME.map(Ast.expr.Name(_, Ast.expr_context.Load)) |
      NUMBER
    )
  }
  def list_contents[_p: P] = P( test.rep(1, ",") ~ ",".? )
  def list[_p: P] = P( list_contents ).map(Ast.expr.List(_, Ast.expr_context.Load))
  def tuple_contents[_p: P] = P( test ~ "," ~ list_contents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  def tuple[_p: P] = P( tuple_contents).map(Ast.expr.Tuple(_, Ast.expr_context.Load))
  def list_comp_contents[_p: P] = P( test ~ comp_for.rep(1) )
  def list_comp[_p: P] = P( list_comp_contents ).map(Ast.expr.ListComp.apply.tupled)
  def generator[_p: P] = P( list_comp_contents ).map(Ast.expr.GeneratorExp.apply.tupled)

  def lambdef[_p: P]: P[Ast.expr.Lambda] = P( kw("lambda") ~ varargslist ~ ":" ~ test ).map(Ast.expr.Lambda.apply.tupled)
  def trailer[_p: P]: P[Ast.expr => Ast.expr] = {
    def call = P("(" ~ arglist ~ ")").map{ case (args, (keywords, starargs, kwargs)) => (lhs: Ast.expr) => Ast.expr.Call(lhs, args, keywords, starargs, kwargs)}
    def slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.expr) => Ast.expr.Subscript(lhs, args, Ast.expr_context.Load))
    def attr = P("." ~ NAME).map(id => (lhs: Ast.expr) => Ast.expr.Attribute(lhs, id, Ast.expr_context.Load))
    P( call | slice | attr )
  }
  def subscriptlist[_p: P] = P( subscript.rep(1, ",") ~ ",".? ).map{
    case Seq(x) => x
    case xs => Ast.slice.ExtSlice(xs)
  }
  def subscript[_p: P]: P[Ast.slice] = {
    def ellipses = P( ("." ~ "." ~ ".").map(_ => Ast.slice.Ellipsis) )
    def single = P( test.map(Ast.slice.Index.apply) )
    def multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      Ast.slice.Slice(
        lower,
        upper,
        step.map(_.getOrElse(Ast.expr.Name(Ast.identifier("None"), Ast.expr_context.Load)))
      )
    }
    P( ellipses | multi | single )
  }

  def sliceop[_p: P] = P( ":" ~ test.? )
  def exprlist[_p: P]: P[Seq[Ast.expr]] = P( expr.rep(1, sep = ",") ~ ",".? )
  def testlist[_p: P]: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") ~ ",".? )
  def dictorsetmaker[_p: P]: P[Ast.expr] = {
    def dict_item = P( test ~ ":" ~ test )
    def dict: P[Ast.expr.Dict] = P(
      (dict_item.rep(1, ",") ~ ",".?).map{x =>
        val (keys, values) = x.unzip
        Ast.expr.Dict(keys, values)
      }
    )
    def dict_comp = P(
      (dict_item ~ comp_for.rep(1)).map(Ast.expr.DictComp.apply.tupled)
    )
    def set: P[Ast.expr.Set] = P( test.rep(1, ",") ~ ",".? ).map(Ast.expr.Set.apply)
    def set_comp = P( test ~ comp_for.rep(1) ).map(Ast.expr.SetComp.apply.tupled)
    P( dict_comp | dict | set_comp | set)
  }

  def arglist[_p: P] = {
    def inits = P( (plain_argument ~ !"=").rep(0, ",") )
    def later = P( named_argument.rep(0, ",") ~ ",".? ~ ("*" ~ test).? ~ ",".? ~ ("**" ~ test).? ~ ",".? ~ named_argument.rep(0, ",")).map{
      case (named1, dot, star, named2) => (named1 ++ named2, dot, star )
    }
    P( inits ~ ",".? ~ later )
  }

  def plain_argument[_p: P] = P( test ~ comp_for.rep ).map{
    case (x, Nil) => x
    case (x, gens) => Ast.expr.GeneratorExp(x, gens)
  }
  def named_argument[_p: P] = P( NAME ~ "=" ~ test  ).map(Ast.keyword.apply.tupled)

  def comp_for[_p: P]: P[Ast.comprehension] = P( kw("for") ~ exprlist ~ kw("in") ~ or_test ~ comp_if.rep ).map{
    case (targets, test, ifs) => Ast.comprehension(tuplize(targets), test, ifs)
  }
  def comp_if[_p: P]: P[Ast.expr] = P( kw("if") ~ test )

  def testlist1[_p: P]: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") )

  // not used in grammar, but may appear in "node" passed from Parser to Compiler
  //  def encoding_decl[_p: P]: P0 = P( NAME )

  def yield_expr[_p: P]: P[Ast.expr.Yield] = P( kw("yield") ~ testlist.map(tuplize).? ).map(Ast.expr.Yield.apply)

  def varargslist[_p: P]: P[Ast.arguments] = {
    def named_arg = P( fpdef ~ ("=" ~ test).? )
    def x = P( named_arg.rep(sep = ",") ~ ",".? ~ ("*" ~ NAME).? ~ ",".? ~ ("**" ~ NAME).? ).map{
      case (normal_args, starargs, kwargs) =>
        val (args, defaults) = normal_args.unzip
        Ast.arguments(args, starargs, kwargs, defaults.flatten)
    }
    P( x )
  }

  def fpdef[_p: P]: P[Ast.expr] = P( NAME.map(Ast.expr.Name(_, Ast.expr_context.Param)) | "(" ~ fplist ~ ")" )
  def fplist[_p: P]: P[Ast.expr] = P( fpdef.rep(sep = ",") ~ ",".? ).map(Ast.expr.Tuple(_, Ast.expr_context.Param))
}
