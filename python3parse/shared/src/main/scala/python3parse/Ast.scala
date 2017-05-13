package python3parse
import acyclic.file

/**
 * A python abstract syntax tree
 *
 * Basically transcribed from https://docs.python.org/3/library/ast.html
 */
object Ast {
  case class identifier(name: String)
  val NoneIdentifier = identifier("None")

  type bool = Boolean
  type int = Int
  type `object` = Any
  type string = String
  type bytes = Array[Byte]
  type constant = Any

  ///    mod = Module(stmt* body)
  ///      | Interactive(stmt* body)
  ///      | Expression(expr body)
  ///
  ///      -- not really an actual node but useful in Jython's typesystem.
  ///      | Suite(stmt* body)
  sealed trait mod
  object mod {
    case class Module(body: Seq[stmt]) extends mod
    case class Interactive(body: Seq[stmt]) extends mod
    case class Expression(body: expr) extends mod
    case class Suite(body: Seq[stmt]) extends mod
  }

  /// stmt = FunctionDef(identifier name, arguments args,
  ///                    stmt* body, expr* decorator_list, expr? returns)
  /// | AsyncFunctionDef(identifier name, arguments args,
  ///                    stmt* body, expr* decorator_list, expr? returns
  /// | ClassDef(identifier name, expr* bases, keyword* keywords,
  ///            stmt* body, expr* decorator_list)
  /// | Return(expr? value)
  /// | Delete(expr* targets)
  /// | Assign(expr* targets, expr value)
  /// | AugAssign(expr target, operator op, expr value)
  /// -- 'simple' indicates that we annotate simple name without parens
  /// | AnnAssign(expr target, expr annotation, expr? value, int simple)
  ///
  /// -- use 'orelse' because else is a keyword in target languages
  /// | For(expr target, expr iter, stmt* body, stmt* orelse)
  /// | AsyncFor(expr target, expr iter, stmt* body, stmt* orelse)
  /// | While(expr test, stmt* body, stmt* orelse)
  /// | If(expr test, stmt* body, stmt* orelse)
  /// | With(withitem* items, stmt* body)
  /// | AsyncWith(withitem* items, stmt* body)
  ///
  /// | Raise(expr? exc, expr? cause)
  /// | Try(stmt* body, excepthandler* handlers, stmt* orelse, stmt* finalbody)
  /// | Assert(expr test, expr? msg)
  ///
  /// | Import(alias* names)
  /// | ImportFrom(identifier? module, alias* names, int? level)
  ///
  /// | Global(identifier* names)
  /// | Nonlocal(identifier* names)
  /// | Expr(expr value)
  /// | Pass | Break | Continue
  sealed trait stmt
  object stmt {
    case class FunctionDef(name: identifier, args: arguments, body: Seq[stmt], decorator_list: Seq[expr], returns: Option[expr]) extends stmt
    case class AsyncFunctionDef(inner: FunctionDef) extends stmt
    case class ClassDef(name: identifier, bases: Seq[expr], keywords: Seq[keyword], body: Seq[stmt], decorator_list: Seq[expr]) extends stmt
    case class Return(value: Option[expr]) extends stmt

    case class Delete(targets: Seq[expr]) extends stmt
    case class Assign(targets: Seq[expr], value: expr) extends stmt
    case class AugAssign(target: expr, op: operator, value: expr) extends stmt
    case class AnnAssign(target: expr, annotation: expr, value: Option[expr], simple: Int) extends stmt

    // use 'orelse' because else is a keyword in target languages
    case class For(target: expr, iter: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
    case class AsyncFor(inner: For) extends stmt
    case class While(test: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
    case class If(test: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
    case class With(items: Seq[withitem], body: Seq[stmt]) extends stmt
    case class AsyncWith(inner: With) extends stmt

    // 'type' is a bad name
    case class Raise(exc: Option[expr], cause: Option[expr]) extends stmt
    case class Try(body: Seq[stmt], handlers: Seq[excepthandler], orelse: Seq[stmt], finalbody: Seq[stmt]) extends stmt
    case class Assert(test: expr, msg: Option[expr]) extends stmt

    case class Import(names: Seq[alias]) extends stmt
    case class ImportFrom(module: Option[identifier], names: Seq[alias], level: Option[int]) extends stmt

    case class Global(names: Seq[identifier]) extends stmt
    case class NonLocal(names: Seq[identifier]) extends stmt
    case class Expr(value: expr) extends stmt
    case object Pass extends stmt
    case object Break extends stmt
    case object Continue extends stmt
  }

  /// BoolOp() can use left & right?
  /// expr = BoolOp(boolop op, expr* values)
  ///    | BinOp(expr left, operator op, expr right)
  ///    | UnaryOp(unaryop op, expr operand)
  ///    | Lambda(arguments args, expr body)
  ///    | IfExp(expr test, expr body, expr orelse)
  ///    | Dict(expr* keys, expr* values)
  ///    | Set(expr* elts)
  ///    | ListComp(expr elt, comprehension* generators)
  ///    | SetComp(expr elt, comprehension* generators)
  ///    | DictComp(expr key, expr value, comprehension* generators)
  ///    | GeneratorExp(expr elt, comprehension* generators)
  ///    -- the grammar constrains where yield expressions can occur
  ///    | Await(expr value)
  ///    | Yield(expr? value)
  ///    | YieldFrom(expr value)
  ///    -- need sequences for compare to distinguish between
  ///    -- x < 4 < 3 and (x < 4) < 3
  ///    | Compare(expr left, cmpop* ops, expr* comparators)
  ///    | Call(expr func, expr* args, keyword* keywords)
  ///    | Num(object n) -- a number as a PyObject.
  ///    | Str(string s) -- need to specify raw, unicode, etc?
  ///    | FormattedValue(expr value, int? conversion, expr? format_spec)
  ///    | JoinedStr(expr* values)
  ///    | Bytes(bytes s)
  ///    | NameConstant(singleton value)
  ///    | Ellipsis
  ///    | Constant(constant value)
  ///    -- the following expression can appear in assignment context
  ///    | Attribute(expr value, identifier attr, expr_context ctx)
  ///    | Subscript(expr value, slice slice, expr_context ctx)
  ///    | Starred(expr value, expr_context ctx)
  ///    | Name(identifier id, expr_context ctx)
  ///    | List(expr* elts, expr_context ctx)
  ///    | Tuple(expr* elts, expr_context ctx)
  sealed trait expr
  object expr {
    case class BoolOp(op: boolop, values: Seq[expr]) extends expr
    case class BinOp(left: expr, op: operator, right: expr) extends expr
    case class UnaryOp(op: unaryop, operand: expr) extends expr
    case class Lambda(args: arguments, body: expr) extends expr
    case class IfExp(test: expr, body: expr, orelse: expr) extends expr
    case class Dict(keys: Seq[expr], values: Seq[expr]) extends expr
    case class Set(elts: Seq[expr]) extends expr
    case class ListComp(elt: expr, generators: Seq[comprehension]) extends expr
    case class SetComp(elt: expr, generators: Seq[comprehension]) extends expr
    case class DictComp(key: expr, value: expr, generators: Seq[comprehension]) extends expr
    case class GeneratorExp(elt: expr, generators: Seq[comprehension]) extends expr
    // the grammar constrains where yield expressions can occur
    case class Await(value: expr) extends expr
    case class Yield(value: Option[expr]) extends expr
    case class YieldFrom(value: expr) extends expr
    // need sequences for compare to distinguish between
    // x < 4 < 3 and (x < 4) < 3
    case class Compare(left: expr, ops: Seq[cmpop], comparators: Seq[expr]) extends expr
    case class Call(func: expr, args: Seq[expr], keywords: Seq[keyword]) extends expr
    case class Num(n: Any) extends expr // a number as a PyObject.
    case class Str(s: string) extends expr // need to raw: specify, unicode, etc?
    case class FormattedValue(value: expr, conversion: Option[int], format_spec: Option[expr]) extends expr
    case class JoinedStr(values: expr) extends expr
    case class Bytes(s: bytes) extends expr
    // NameConstant
    case class Ellipsis() extends expr
    val EllipsisValue = Ellipsis()
    case class Constant(value: constant) extends expr

    // the following expression can appear in assignment context
    case class Attribute(value: expr, attr: identifier, ctx: expr_context) extends expr
    case class Subscript(value: expr, slice: slice, ctx: expr_context) extends expr
    case class Starred(value: expr, ctx: expr_context) extends expr
    case class Name(id: identifier, ctx: expr_context, annot: Option[expr] = None) extends expr
    val NoneName = Name(NoneIdentifier, expr_context.Load)
    case class List(elts: Seq[expr], ctx: expr_context) extends expr
    case class Tuple(elts: Seq[expr], ctx: expr_context) extends expr
  }
  // col_offset is the byte offset in the utf8 string the parser uses
  case class attributes(lineno: Int, col_offset: Int)

  /// expr_context = Load | Store | Del | AugLoad | AugStore | Param
  sealed trait expr_context
  object expr_context {
    case object Load extends expr_context
    case object Store extends expr_context
    case object Del extends expr_context
    case object AugLoad extends expr_context
    case object AugStore extends expr_context
    case object Param extends expr_context
  }

  /// slice = Slice(expr? lower, expr? upper, expr? step)
  ///       | ExtSlice(slice* dims)
  ///       | Index(expr value)
  sealed trait slice
  object slice {
    case class Slice(lower: Option[expr], upper: Option[expr], step: Option[expr]) extends slice
    case class ExtSlice(dims: Seq[slice]) extends slice
    case class Index(value: expr) extends slice
  }

  // boolop = And | Or
  sealed trait boolop
  object boolop {
    case object And extends boolop
    case object Or extends boolop
  }

  /// operator = Add | Sub | Mult | MatMult | Div | Mod | Pow | LShift
  ///               | RShift | BitOr | BitXor | BitAnd | FloorDiv
  sealed trait operator
  case object operator {
    case object Add extends operator
    case object Sub  extends operator
    case object Mult  extends operator
    case object MatMult  extends operator
    case object Div  extends operator
    case object Mod  extends operator
    case object Pow  extends operator
    case object LShift  extends operator
    case object RShift  extends operator
    case object BitOr  extends operator
    case object BitXor  extends operator
    case object BitAnd  extends operator
    case object FloorDiv extends operator
  }

  /// unaryop = Invert | Not | UAdd | USub
  sealed trait unaryop
  object unaryop {
    case object Invert extends unaryop
    case object Not extends unaryop
    case object UAdd extends unaryop
    case object USub extends unaryop
  }

  /// cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
  sealed trait cmpop
  object cmpop {
    case object Eq extends cmpop
    case object NotEq extends cmpop
    case object Lt extends cmpop
    case object LtE extends cmpop
    case object Gt extends cmpop
    case object GtE extends cmpop
    case object Is extends cmpop
    case object IsNot extends cmpop
    case object In extends cmpop
    case object NotIn extends cmpop
  }

  /// comprehension = (expr target, expr iter, expr* ifs, int is_async)
  case class comprehension(target: expr, iter: expr, ifs: Seq[expr], is_async: Boolean = false) extends expr

  /// excepthandler = ExceptHandler(expr? type, identifier? name, stmt* body)
  ///                 attributes (int lineno, int col_offset)
  sealed trait excepthandler
  object excepthandler{
    case class ExceptHandler(`type`: Option[expr], name: Option[identifier], body: Seq[stmt]) extends excepthandler
  }

  /// arguments = (arg* args, arg? vararg, arg* kwonlyargs, expr* kw_defaults,
  ///              arg? kwarg, expr* defaults)
  case class arguments(args: Seq[arg], vararg: Option[arg], kwonlyargs: Seq[arg], kw_defaults: Seq[expr],
                       kwarg: Option[arg], defaults: Seq[expr])

  case object   arguments {
    val empty = arguments(Seq(), None, Seq(), Seq(), None, Seq())
  }

  /// arg = (identifier arg, expr? annotation)
  ///       attributes (int lineno, int col_offset)
  case class arg(arg: identifier, annotation: Option[expr] = None)

  /// -- keyword arguments supplied to call (NULL identifier for **kwargs)
  /// keyword = (identifier? arg, expr value)
  case class keyword(arg: Option[identifier], value: expr)

  /// --- import name with optional 'as' alias.
  /// alias = (identifier name, identifier? asname)
  case class alias(name: identifier, asname: Option[identifier])

  /// withitem = (expr context_expr, expr? optional_vars)
  case class withitem(context_expr: expr, optional_vars: Option[expr])
}