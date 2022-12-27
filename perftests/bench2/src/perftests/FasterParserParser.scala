package perftests

import fastparse.JsonnetWhitespace._
import fastparse._
import Expr.Member.Visibility
import scala.annotation.switch

object fastparseParser{
  val precedenceTable = Seq(
    Seq("*", "/", "%"),
    Seq("+", "-"),
    Seq("<<", ">>"),
    Seq("<", ">", "<=", ">=", "in"),
    Seq("==", "!="),
    Seq("&"),
    Seq("^"),
    Seq("|"),
    Seq("&&"),
    Seq("||")
  )

  val precedence = precedenceTable
    .reverse
    .zipWithIndex
    .flatMap{case (ops, idx) => ops.map(_ -> idx)}
    .toMap

  val keywords = Set(
    "assert", "else", "error", "false", "for", "function", "if", "import", "importstr",
    "in", "local", "null", "tailstrict", "then", "self", "super", "true"
  )

  def idStartChar(c: Char) = c == '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  def id[_p: P] = P(
    CharIn("_a-zA-Z0-9") ~~
    CharsWhileIn("_a-zA-Z0-9", 0)
  ).!.filter(s => !keywords.contains(s))

  def break[_p: P] = P(!CharIn("_a-zA-Z0-9"))
  def number[_p: P]: P[Expr.Num] = P(
    Index ~~ (
      CharsWhileIn("0-9") ~~
        ("." ~ CharsWhileIn("0-9")).? ~~
        (CharIn("eE") ~ CharIn("+\\-").? ~~ CharsWhileIn("0-9")).?
      ).!
  ).map(s => Expr.Num(s._1, s._2.toDouble))

  def escape[_p: P] = P( escape0 | escape1 )
  def escape0[_p: P] = P("\\" ~~ !"u" ~~ AnyChar.!).map{
    case "\"" => "\""
    case "'" => "\'"
    case "\\" => "\\"
    case "/" => "/"
    case "b" => "\b"
    case "f" => "\f"
    case "n" => "\n"
    case "r" => "\r"
    case "t" => "\t"
  }
  def escape1[_p: P] = P( "\\u" ~~ CharIn("0-9").repX(min=4, max=4).! ).map{
    s => Integer.parseInt(s, 16).toChar.toString
  }
  def doubleString[_p: P]: P[Seq[String]] =
    P( (CharsWhile(x => x != '"' && x != '\\').! | escape).repX ~~ "\"" )
  def singleString[_p: P]: P[Seq[String]] =
    P( (CharsWhile(x => x != '\'' && x != '\\').! | escape).repX ~~ "'" )
  def literalDoubleString[_p: P]: P[Seq[String]] =
    P( (CharsWhile(_ != '"').! | "\"\"".!.map(_ => "\"")).repX ~~ "\""  )
  def literalSingleString[_p: P]: P[Seq[String]] =
    P( (CharsWhile(_ != '\'').! | "''".!.map(_ => "'")).repX ~~ "'" )

  def tripleBarStringLines[_p: P]: P[Seq[String]] = P(
    tripleBarStringHead.flatMapX { case (pre, w, head) =>
      tripleBarStringBody(w).map(pre ++ Seq(head, "\n") ++ _)
    }
  )
  def tripleBarString[_p: P]: P[Seq[String]] = P(
    "||"./ ~~ CharsWhileIn(" \t", 0) ~~ "\n" ~~ tripleBarStringLines ~~ "\n" ~~ CharsWhileIn(" \t") ~~ "|||"
  )
  def string[_p: P]: P[String] = P(
    SingleChar.flatMapX{
      case '\"' => doubleString
      case '\'' => singleString
      case '@' => SingleChar./.flatMapX{
        case '\"' => literalDoubleString
        case '\'' => literalSingleString
        case _ => Fail
      }
      case '|' => tripleBarString
      case _ => Fail
    }
  ).map(_.mkString)

  def tripleBarStringHead[_p: P] = P(
    (CharsWhileIn(" \t", 0) ~~ "\n".!).repX ~~
      CharsWhileIn(" \t", 1).! ~~
      CharsWhile(_ != '\n').!
  )
  def tripleBarBlankHead[_p: P]: P[String] =
    P( CharsWhileIn(" \t", 0) ~~ &("\n").map(_ => "\n") )

  def tripleBarBlank[_p: P]: P[String] = P( "\n" ~~ tripleBarBlankHead )

  def tripleBarStringBody[_p: P](w: String): P[Seq[String]] = P (
    (tripleBarBlank | "\n" ~~ w ~~ CharsWhile(_ != '\n').!.map(_ + "\n")).repX
  )


  def obj[_p: P]: P[Expr] = P( (Index ~~ objinside).map(Expr.Obj.tupled) )
  def arr[_p: P]: P[Expr] = P( (Index ~~ &("]")).map(Expr.Arr(_, Nil)) | arrBody )
  def compSuffix[_p: P] = P( forspec ~ compspec ).map(Left(_))
  def arrBody[_p: P]: P[Expr] = P(
    Index ~~ expr ~ (compSuffix | "," ~ (compSuffix | (expr.rep(0, sep = ",") ~ ",".?).map(Right(_)))).?
  ).map{
    case (offset, first, None) => Expr.Arr(offset, Seq(first))
    case (offset, first, Some(Left(comp))) => Expr.Comp(offset, first, comp._1, comp._2)
    case (offset, first, Some(Right(rest))) => Expr.Arr(offset, Seq(first) ++ rest)
  }

  def assertExpr[_p: P](index: Int): P[Expr] = P( assertStmt ~ ";" ~ expr ).map(t => Expr.AssertExpr(index, t._1, t._2))
  def function[_p: P](index: Int): P[Expr] = P( "(" ~/ params ~ ")" ~ expr ).map(t => Expr.Function(index, t._1, t._2))
  def ifElse[_p: P](index: Int): P[Expr] = P( Index ~~ expr ~ "then" ~~ break ~ expr ~ ("else" ~~ break ~ expr).? ).map(Expr.IfElse.tupled)
  def localExpr[_p: P]: P[Expr] = P( Index ~~ bind.rep(min=1, sep = ","./) ~ ";" ~ expr ).map(Expr.LocalExpr.tupled)

  def expr[_p: P]: P[Expr] = P("" ~ expr1 ~ (Index ~~ binaryop ~/ expr1).rep ~ "").map{ case (pre, fs) =>
    var remaining = fs
    def climb(minPrec: Int, current: Expr): Expr = {
      var result = current
      while(
        remaining.headOption match{
          case None => false
          case Some((offset, op, next)) =>
            val prec: Int = precedence(op)
            if (prec < minPrec) false
            else{
              remaining = remaining.tail
              val rhs = climb(prec + 1, next)
              val op1 = op match{
                case "*" => Expr.BinaryOp.`*`
                case "/" => Expr.BinaryOp.`/`
                case "%" => Expr.BinaryOp.`%`
                case "+" => Expr.BinaryOp.`+`
                case "-" => Expr.BinaryOp.`-`
                case "<<" => Expr.BinaryOp.`<<`
                case ">>" => Expr.BinaryOp.`>>`
                case "<" => Expr.BinaryOp.`<`
                case ">" => Expr.BinaryOp.`>`
                case "<=" => Expr.BinaryOp.`<=`
                case ">=" => Expr.BinaryOp.`>=`
                case "in" => Expr.BinaryOp.`in`
                case "==" => Expr.BinaryOp.`==`
                case "!=" => Expr.BinaryOp.`!=`
                case "&" => Expr.BinaryOp.`&`
                case "^" => Expr.BinaryOp.`^`
                case "|" => Expr.BinaryOp.`|`
                case "&&" => Expr.BinaryOp.`&&`
                case "||" => Expr.BinaryOp.`||`
              }
              result = Expr.BinaryOp(offset, result, op1, rhs)
              true
            }
        }
      )()
      result
    }

    climb(0, pre)
  }

  def expr1[_p: P]: P[Expr] = P(expr2 ~ exprSuffix2.rep).map{
    case (pre, fs) => fs.foldLeft(pre){case (p, f) => f(p) }
  }

  def exprSuffix2[_p: P]: P[Expr => Expr] = P(
    for{
      i <- Index
      c <- CharIn(".[({").!.map(_(0))
      r <- (c: @switch) match{
        case '.' => Pass ~ id.map(x => Expr.Select(i, _: Expr, x))
        case '[' => Pass ~ (expr.? ~ (":" ~ expr.?).rep ~ "]").map{
          case (Some(tree), Seq()) => Expr.Lookup(i, _: Expr, tree)
          case (start, ins) => Expr.Slice(i, _: Expr, start, ins.lift(0).flatten, ins.lift(1).flatten)
        }
        case '(' => Pass ~ (args ~ ")").map(x => Expr.Apply(i, _: Expr, x))
        case '{' => Pass ~ (objinside ~ "}").map(x => Expr.ObjExtend(i, _: Expr, x))
        case _ => Fail
      }
    } yield r
  )

  def local[_p: P] = P( localExpr )
  def parened[_p: P] = P( (Index ~~ expr).map(Expr.Parened.tupled) )
  def importStr[_p: P](index: Int) = P( string.map(Expr.ImportStr(index, _)) )
  def `import`[_p: P](index: Int) = P( string.map(Expr.Import(index, _)) )
  def error[_p: P](index: Int) = P(expr.map(Expr.Error(index, _)) )
  def strExpr[_p: P] = P((Index ~~ string).map(Expr.Str.tupled))
  def idExpr[_p: P] = P( (Index ~~ id).map(Expr.Id.tupled) )
  def unaryOpExpr[_p: P](index: Int, op: Char) = P(
    expr1.map{ e =>
      def k2 = op match{
        case '+' => Expr.UnaryOp.`+`
        case '-' => Expr.UnaryOp.`-`
        case '~' => Expr.UnaryOp.`~`
        case '!' => Expr.UnaryOp.`!`
      }
      Expr.UnaryOp(index, k2, e)
    }
  )

  def constructString(index: Int, lines: Seq[String]) = Expr.Str(index, lines.mkString)
  // Any `expr` that isn't naively left-recursive
  def expr2[_p: P]: P[Expr] = P(
    Index.flatMapX{ index =>
      SingleChar.flatMapX{ c =>
        (c: @switch) match {
          case '{' => Pass ~ obj ~ "}"
          case '+' | '-' | '~' | '!' => Pass ~ unaryOpExpr(index, c)
          case '[' => Pass ~ arr ~ "]"
          case '(' => Pass ~ parened ~ ")"
          case '\"' => doubleString.map(constructString(index, _))
          case '\'' => singleString.map(constructString(index, _))
          case '@' => SingleChar./.flatMapX{
            case '\"' => literalDoubleString.map(constructString(index, _))
            case '\'' => literalSingleString.map(constructString(index, _))
            case _ => Fail
          }
          case '|' => tripleBarString.map(constructString(index, _))
          case '$' => Pass(Expr.$(index))
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            P.current.index = index; number
          case x if idStartChar(x) => CharsWhileIn("_a-zA-Z0-9", 0).!.flatMapX { y =>
            x + y match {
              case "null"      => Pass(Expr.Null(index))
              case "true"      => Pass(Expr.True(index))
              case "false"     => Pass(Expr.False(index))
              case "self"      => Pass(Expr.Self(index))
              case "super"     => Pass(Expr.Super(index))
              case "if"        => Pass ~ ifElse(index)
              case "function"  => Pass ~ function(index)
              case "importStr" => Pass ~ importStr(index)
              case "import"    => Pass ~ `import`(index)
              case "error"     => Pass ~ error(index)
              case "assert"    => Pass ~ assertExpr(index)
              case "local"     => Pass ~ local
              case x           => Pass(Expr.Id(index, x))
            }
          }
          case _ => Fail
        }
      }
    }
  )

  def objinside[_p: P]: P[Expr.ObjBody] = P(
    member.rep(sep = ",") ~ ",".? ~ (forspec ~ compspec).?
  ).map{
    case (exprs, None) => Expr.ObjBody.MemberList(exprs)
    case (exprs, Some(comps)) =>
      val preLocals = exprs.takeWhile(_.isInstanceOf[Expr.Member.BindStmt]).map(_.asInstanceOf[Expr.Member.BindStmt])
      val Expr.Member.Field(offset, Expr.FieldName.Dyn(lhs), false, None, Visibility.Normal, rhs) =
        exprs(preLocals.length)
      val postLocals = exprs.drop(preLocals.length+1).takeWhile(_.isInstanceOf[Expr.Member.BindStmt])
        .map(_.asInstanceOf[Expr.Member.BindStmt])
      Expr.ObjBody.ObjComp(preLocals, lhs, rhs, postLocals, comps._1, comps._2)
  }

  def member[_p: P]: P[Expr.Member] = P( objlocal | assertStmt | field )
  def field[_p: P] = P(
    (Index ~~ fieldname ~/ "+".!.? ~ ("(" ~ params ~ ")").? ~ fieldKeySep ~/ expr).map{
      case (offset, name, plus, p, h2, e) =>
        Expr.Member.Field(offset, name, plus.nonEmpty, p, h2, e)
    }
  )
  def fieldKeySep[_p: P] = P( StringIn(":::", "::", ":") ).!.map{
    case ":" => Visibility.Normal
    case "::" => Visibility.Hidden
    case ":::" => Visibility.Unhide
  }
  def objlocal[_p: P] = P( "local" ~~ break ~/ bind ).map(Expr.Member.BindStmt)
  def compspec[_p: P]: P[Seq[Expr.CompSpec]] = P( (forspec | ifspec).rep )
  def forspec[_p: P] = P( Index ~~ "for" ~~ break ~/ id ~ "in" ~~ break ~ expr ).map(Expr.ForSpec.tupled)
  def ifspec[_p: P] = P( Index ~~ "if" ~~ break  ~/ expr ).map(Expr.IfSpec.tupled)
  def fieldname[_p: P] = P( id.map(Expr.FieldName.Fixed) | string.map(Expr.FieldName.Fixed) | "[" ~ expr.map(Expr.FieldName.Dyn) ~ "]" )
  def assertStmt[_p: P] = P( "assert" ~~ break  ~/ expr ~ (":" ~ expr).? ).map(Expr.Member.AssertStmt.tupled)
  def bind[_p: P] = P( Index ~~ id ~ ("(" ~/ params.? ~ ")").?.map(_.flatten) ~ "=" ~ expr ).map(Expr.Bind.tupled)
  def args[_p: P] = P( ((id ~ "=").? ~ expr).rep(sep = ",") ~ ",".? ).flatMap{ x =>
    if (x.sliding(2).exists{case Seq(l, r) => l._1.isDefined && r._1.isEmpty case _ => false}) {
      Fail
    } else Pass.map(_ => Expr.Args(x))
  }

  def params[_p: P]: P[Expr.Params] = P( (id ~ ("=" ~ expr).?).rep(sep = ",") ~ ",".? ).flatMap{ x =>
    val seen = collection.mutable.Set.empty[String]
    var overlap: String = null
    for((k, v) <- x){
      if (seen(k)) overlap = k
      else seen.add(k)
    }
    if (overlap == null) Pass.map(_ => Expr.Params(x))
    else Fail

  }

  def binaryop[_p: P] = P(
    StringIn(
      "<<", ">>", "<=", ">=", "in", "==", "!=", "&&", "||",
      "*", "/", "%", "+", "-", "<", ">", "&", "^", "|"
    )

  ).!

  def unaryop[_p: P]	= P( CharIn("\\-+!~") ).!

  def document[_p: P]: P[Expr] = P( expr ~ End )
}
