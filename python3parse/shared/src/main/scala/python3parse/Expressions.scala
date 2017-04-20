package python3parse

import fastparse.core.Implicits.Sequencer
import fastparse.noApi._
import WsApi._
import Lexical.kw
import fastparse.all.ParseCtx
import fastparse.core
import python3parse.Expressions.tfpdef

import scala.collection.mutable

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
    case xs => Ast.expr.Tuple(xs, getContext())
  }
  def tuplize(xs: Seq[Ast.expr], context: Ast.expr_context) = xs match{
    case Seq(x) => x
    case xs => Ast.expr.Tuple(xs, context)
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

  private val contextStack = new mutable.Stack[Ast.expr_context]()
  def getContext(): Ast.expr_context = {
    if (contextStack.isEmpty) Ast.expr_context.Load
    else contextStack.head
  }

  /// star_expr: '*' expr
  val star_expr: P[Ast.expr.Starred] = P( "*" ~ expr ).map(expr => Ast.expr.Starred(expr, getContext()))

  val Add = op("+", Ast.operator.Add)
  val Sub = op("-", Ast.operator.Sub)
  val Pow = op("**", Ast.operator.Pow)
  val Mult= op("*", Ast.operator.Mult)
  val MatrixMult = op("@", Ast.operator.MatMult)
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
  val power: P[Ast.expr] = P( atom_expr ~ (Pow ~ factor).? ).map {
    case (lhs, rhs) =>
      rhs match {
        case None => lhs
        case Some((op, right)) => Ast.expr.BinOp(lhs, op, right)
      }
  }

  /// atom_expr: [AWAIT] atom trailer*
  val atom_expr: P[Ast.expr] = P( "await".!.? ~ atom ~ trailer.rep()).map {
    case (await, atom, trailers) =>
      val expr = trailers.foldLeft(atom)((l, t) => t(l))
      await match {
        case None => expr
        case Some(_) => Ast.expr.Await(expr)
      }
  }

  /// atom: ('(' [yield_expr|testlist_comp] ')' |
  ///        '[' [testlist_comp] ']' |
  ///        '{' [dictorsetmaker] '}' |
  ///        NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
  val atom: P[Ast.expr] = {
    val empty_tuple = ("(" ~ ")").map(_ => Ast.expr.Tuple(Nil, Ast.expr_context.Load))
    val empty_list = ("[" ~ "]").map(_ => Ast.expr.List(Nil, Ast.expr_context.Load))
    val empty_dict = ("{" ~ "}").map(_ => Ast.expr.Dict(Nil, Nil))
    P(
      empty_tuple  |
      empty_list |
      empty_dict |
      "(" ~ (yield_expr | testlist_comp(TupleFactory)) ~ ")" |
      "[" ~ testlist_comp(ListFactory) ~ "]" |
      "{" ~ dictorsetmaker ~ "}" |
      STRING.rep(1).map(_.mkString).map(Ast.expr.Str) |
      NAME.map(id => Ast.expr.Name(id, getContext())) |
      NUMBER
    )
  }

  trait SeqFactory {
    def seqToExpr(seq: Seq[Ast.expr], trailingComma: Option[String]): Ast.expr
    def compToExpr(elem: Ast.expr, comps: Seq[Ast.comprehension]): Ast.expr
  }

  case object ListFactory extends SeqFactory {
    def seqToExpr(seq: Seq[Ast.expr], trailingComma: Option[String]) = Ast.expr.List(seq, getContext())
    def compToExpr(elem: Ast.expr, comps: Seq[Ast.comprehension]) = Ast.expr.ListComp(elem, comps)
  }

  case object TupleFactory extends SeqFactory {
    def seqToExpr(seq: Seq[Ast.expr], trailingComma: Option[String]) = {
      if (seq.size == 1 && trailingComma.isEmpty)
        seq.head
      else
        Ast.expr.Tuple(seq, getContext())
    }

    def compToExpr(elem: Ast.expr, comps: Seq[Ast.comprehension]) = Ast.expr.GeneratorExp(elem, comps)
  }

  case object SetFactory extends SeqFactory {
    def seqToExpr(seq: Seq[Ast.expr], trailingComma: Option[String]) = Ast.expr.Set(seq)
    def compToExpr(elem: Ast.expr, comps: Seq[Ast.comprehension]) = Ast.expr.SetComp(elem, comps)
  }

  /// testlist_comp: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )
  def testlist_comp(factory: SeqFactory): P[Ast.expr] = {
    val test_or_star = P( test | star_expr )
    val seq = P( test_or_star.rep(1, ",") ~ ",".!.?).map((factory.seqToExpr _).tupled)
    val seq_comp = P( test_or_star ~ comp_for ).map((factory.compToExpr _).tupled)
    P( seq_comp | seq )
  }

  /// trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
  val trailer: P[Ast.expr => Ast.expr] = {
    val call = P("(" ~ arglist.? ~ ")").map {
      case None =>
        (lhs: Ast.expr) => Ast.expr.Call(lhs, Nil, Nil)
      case Some(arglist) =>
        (lhs: Ast.expr) => Ast.expr.Call(lhs, arglist._1, arglist._2)
    }
    val slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.expr) => Ast.expr.Subscript(lhs, args, getContext()))
    val attr = P("." ~ NAME).map(id => (lhs: Ast.expr) => Ast.expr.Attribute(lhs, id, getContext()))
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
        step.map(_.getOrElse(Ast.expr.NoneName))
      )
    }
    P( multi | single )
  }

  /// sliceop: ':' [test]
  val sliceop = P( ":" ~ test.? )

  /// exprlist: (expr|star_expr) (',' (expr|star_expr))* [',']
  val exprlist: P[Seq[Ast.expr]] = P( (expr | star_expr).rep(1, sep = ",") ~ ",".? )

  object storingExprlist extends core.Parser[Seq[Ast.expr], Char, String] {
    override def parseRec(cfg: ParseCtx, index: Int) = {
      contextStack.push(Ast.expr_context.Store)
      try {
        exprlist.parseRec(cfg, index)
      } finally {
        contextStack.pop()
      }
    }
  }

  /// testlist: test (',' test)* [',']
  val testlist: P[Seq[Ast.expr]] = P( test.rep(1, sep = ",") ~ ",".? )

  /// testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
  val testlist_star_expr: P[Seq[Ast.expr]] = P( (test|star_expr).rep(1, sep = ",") ~ ",".? )

  /// dictorsetmaker: ( ((test ':' test | '**' expr)
  ///                    (comp_for | (',' (test ':' test | '**' expr))* [','])) |
  ///                   ((test | star_expr)
  ///                    (comp_for | (',' (test | star_expr))* [','])) )
  val dictorsetmaker: P[Ast.expr] = {
    val key_value: P[(Ast.expr, Ast.expr)] = P( test ~ ":" ~ test )
    val starstar: P[(Ast.expr, Ast.expr)] = P( "**" ~ expr ).map(expr => (Ast.expr.NoneName, expr))
    val dict_item: P[(Ast.expr, Ast.expr)] = P( key_value | starstar )
    val dict: P[Ast.expr.Dict] = P( dict_item.rep(1, ",") ~ ",".? ).map { list =>
      Ast.expr.Dict.tupled(list.unzip)
    }
    val dict_comp = P(
      (dict_item ~ comp_for).map(Ast.expr.DictComp.tupled)
    )
    P( dict_comp | dict | testlist_comp(SetFactory))
  }

  /// arglist: argument (',' argument)*  [',']
  val arglist: P[(Seq[Ast.expr], Seq[Ast.keyword])] = P( argument.rep(1, ",") ~ ",".? ).map { seq =>
    val (part_args, part_keywords) = seq.partition(_.isLeft)
    val args = part_args.map(_.left.get)
    val keywords = part_keywords.map(_.right.get)
    (args, keywords)
  }

  /// argument: ( test [comp_for] |
  ///             test '=' test |  # meant: NAME '=' test
  ///             '**' test |
  ///             '*' test )
  val argument: P[Either[Ast.expr, Ast.keyword]] = {
    val arg_comp = P( test ~ comp_for.? ).map {
      case (x, None) => Left(x)
      case (x, Some(y)) => Left(Ast.expr.GeneratorExp(x, y))
    }
    val arg_named = P( NAME ~ "=" ~ test  ).map{ case (n, t) => Right(Ast.keyword(Some(n), t)) }
    val arg_starstar = P( "**" ~ test).map(x => Right(Ast.keyword(None, x)))
    val arg_star = P( "*" ~ test).map(x => Left(Ast.expr.Starred(x, getContext())))
    P( arg_named | arg_starstar | arg_star | arg_comp )
  }

  /// comp_for: [ASYNC] 'for' exprlist 'in' or_test [comp_iter]
  /// comp_iter: comp_for | comp_if
  /// comp_if: 'if' test_nocond [comp_iter]
  val comp_for: P[Seq[Ast.comprehension]] = {
    val comp_if_rep: P[Seq[Ast.expr]] = P( P( "if" ~ test_nocond ).rep(1) )
    val comp_iter = P( (comp_if_rep ~ comp_for.?).map(t => Left(t)) | comp_for.map(Right(_)) )
    P( "async".!.? ~ "for" ~ storingExprlist ~ "in" ~ or_test ~ comp_iter.? ).map{
      case (async, targets, test, iter) =>
        val trgs = tuplize(targets, Ast.expr_context.Store)
        val c0 = Ast.comprehension(trgs, iter = test, ifs = Nil, is_async = async.isDefined)
        iter match {
          case Some(Left( (ifs, None) )) =>
            Seq(c0.copy(ifs = ifs))
          case Some(Left( (ifs, Some(cn) ))) =>
            c0.copy(ifs = ifs) +: cn
          case Some(Right(cn)) =>
            c0 +: cn
          case None =>
            Seq(c0)
        }
    }
  }

  /// # not used in grammar, but may appear in "node" passed from Parser to Compiler
  //val encoding_decl = P( NAME )

  /// yield_expr: 'yield' [yield_arg]
  val yield_expr: P[Ast.expr.Yield] = P( kw("yield") ~ yield_arg.? ).map {
    case None => Ast.expr.Yield(None)
    case Some(list) =>
      val expr = if (list.size == 1) list.head else Ast.expr.Tuple(list, getContext())
      Ast.expr.Yield(Some(expr))
  }

  /// yield_arg: 'from' test | testlist
  val yield_arg: P[Seq[Ast.expr]] = {
    val from = P( "from" ~ test ).map(x => Seq(Ast.expr.YieldFrom(x)))
    P( from | testlist )
  }

  /// typedargslist: (tfpdef ['=' test] (',' tfpdef ['=' test])* [',' [
  ///             '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
  ///          | '**' tfpdef [',']]]
  ///   | '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
  ///   | '**' tfpdef [','])
  val typedargslist: P[Ast.arguments] = {
    type NamedArg = (Ast.arg, Option[Ast.expr])
    val named_arg: P[NamedArg] = P( tfpdef ~ ("=" ~ test).? )
    val named_args: P[Seq[NamedArg]] = P( named_arg.rep(sep = ",") )
    type StarArgs = (Option[Ast.arg], Seq[NamedArg], Option[Ast.arg])
    val starstar_args: P[StarArgs] = P( "**" ~ tfpdef ).map(kwarg => (None, Nil, Some(kwarg)))
    val named_star_args: P[StarArgs] = P( named_args ~ ("," ~ starstar_args).? ).map {
      case (nargs, None) => (None, nargs, None)
      case (nargs, Some(kwarg)) => (None, nargs, kwarg._3)
    }
    val sub_star_args: P[StarArgs] = P( starstar_args | named_star_args )
    val star_args: P[StarArgs] = P( "*" ~ tfpdef.? ~ ("," ~ sub_star_args).?).map {
      case (vararg, None) => (vararg, Nil, None)
      case (vararg, Some(sub)) => (vararg, sub._2, sub._3)
    }
    val alt1 = P( named_args ~ ("," ~ (starstar_args | star_args)).? ~ ",".?).map {
      case (normal, star) =>
        val (args, defaults) = normal.unzip
        val (vararg, nargs, kwarg) = star.getOrElse((None, Nil, None))
        val (kwonlyargs, kw_defaults) = nargs.unzip
        Ast.arguments(args = args, vararg = vararg, kwonlyargs = kwonlyargs, kw_defaults = kw_defaults.flatten,
          kwarg = kwarg, defaults = defaults.flatten)
    }
    val alt2 = P( (starstar_args | star_args) ~ ",".?).map {
      case (vararg, nargs, kwarg) =>
        val (kwonlyargs, kw_defaults) = nargs.unzip
        Ast.arguments(args = Nil, vararg = vararg, kwonlyargs = kwonlyargs, kw_defaults = kw_defaults.flatten,
          kwarg = kwarg, defaults = Nil)
    }
    P( alt2 | alt1 )
  }

  /// tfpdef: NAME [':' test]
  val tfpdef: P[Ast.arg] = P( NAME ~ (":" ~ test).?).map {
    case (identifier, annot) =>
      Ast.arg(identifier, annot)
  }

  /// varargslist: (vfpdef ['=' test] (',' vfpdef ['=' test])* [',' [
  ///          '*' [vfpdef] (',' vfpdef ['=' test])* [',' ['**' vfpdef [',']]]
  ///       | '**' vfpdef [',']]]
  ///   | '*' [vfpdef] (',' vfpdef ['=' test])* [',' ['**' vfpdef [',']]]
  ///   | '**' vfpdef [',']
  /// )
  val varargslist: P[Ast.arguments] = {
    type NamedArg = (Ast.arg, Option[Ast.expr])
    val named_arg: P[NamedArg] = P( vfpdef ~ ("=" ~ test).? )
    val named_args: P[Seq[NamedArg]] = P( named_arg.rep(sep = ",") )
    type StarArgs = (Option[Ast.arg], Seq[NamedArg], Option[Ast.arg])
    val starstar_args: P[StarArgs] = P( "**" ~ vfpdef ).map(kwarg => (None, Nil, Some(kwarg)))
    val named_star_args: P[StarArgs] = P( named_args ~ ("," ~ starstar_args).? ).map {
      case (nargs, None) => (None, nargs, None)
      case (nargs, Some(kwarg)) => (None, nargs, kwarg._3)
    }
    val sub_star_args: P[StarArgs] = P( starstar_args | named_star_args )
    val star_args: P[StarArgs] = P( "*" ~ vfpdef.? ~ ("," ~ sub_star_args).?).map {
      case (vararg, None) => (vararg, Nil, None)
      case (vararg, Some(sub)) => (vararg, sub._2, sub._3)
    }
    val alt1 = P( named_args ~ ("," ~ (starstar_args | star_args)).? ~ ",".?).map {
      case (normal, star) =>
        val (args, defaults) = normal.unzip
        val (vararg, nargs, kwarg) = star.getOrElse((None, Nil, None))
        val (kwonlyargs, kw_defaults) = nargs.unzip
        Ast.arguments(args = args, vararg = vararg, kwonlyargs = kwonlyargs, kw_defaults = kw_defaults.flatten,
          kwarg = kwarg, defaults = defaults.flatten)
    }
    val alt2 = P( (starstar_args | star_args) ).map {
      case (vararg, nargs, kwarg) =>
        val (kwonlyargs, kw_defaults) = nargs.unzip
        Ast.arguments(args = Nil, vararg = vararg, kwonlyargs = kwonlyargs, kw_defaults = kw_defaults.flatten,
          kwarg = kwarg, defaults = Nil)
    }
    P( alt2 | alt1 )
  }

  /// vfpdef: NAME
  val vfpdef: P[Ast.arg] = P( NAME ).map { identifier =>
    Ast.arg(identifier)
  }
}
