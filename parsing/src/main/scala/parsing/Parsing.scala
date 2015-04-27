package parsing

import parsing.Parsing.Res.Success

import scala.annotation.tailrec
import scala.collection.mutable

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
case class FuncName(name: String)
object Parsing {

  implicit def enclosingFunctionName: FuncName = macro impl

  def impl(c: Context): c.Expr[FuncName] = {
    import c.universe._
    val sym = c.internal.enclosingOwner
    c.Expr[FuncName](q"parsing.FuncName(${sym.name.toString})")
  }

  sealed trait Res[+T]
  object Res{
    case class Success[T](t: T, index: Int) extends Res[T]
    case class Failure(index: Int, ps: List[Parser[_]]) extends Res[Nothing]
  }
  import Res._
  sealed trait Parser[+T]{
    def log(msg: String) = Parser.Logged(this, msg)
    def parse(input: String, index: Int): Res[T]
    def rep = Parser.Repeat(this, 0, Parser.Pass)
    def rep1 = Parser.Repeat(this, 1, Parser.Pass)
    def rep(delimiter: Parser[_]): Parser[Seq[T]] = Parser.Repeat(this, 0, delimiter)
    def rep1(delimiter: Parser[_]): Parser[Seq[T]] = Parser.Repeat(this, 1, delimiter)

    def |[T1 >: T, V <: T1](p: Parser[V]) = Parser.Either(this, p)
    def ~[V](p: Parser[V]) = Parser.Sequence(this, p)
    def ? = Parser.Either(this, Parser.Pass)
    def unary_! = Parser.Not(this)
  }
  def &(p: Parser[_]): Parser[Unit] = Parser.Lookahead(p)
  object Parser{
    var logNesting = 0
    case class Logged[+T](p: Parser[T], msg: String) extends Parser[T]{
      def parse(input: String, index: Int) = {
        val indent = "  " * logNesting
        println(indent + "+" + msg + ":" + index)
        logNesting += 1
        val res = p.parse(input, index)
        logNesting -= 1
        println(indent + "-" + msg + ":" + res)
        res
      }
    }
    case class Lazy[+T](name: String, p: () => Parser[T]) extends Parser[T]{
      lazy val pCached = p()
      def parse(input: String, index: Int) = {
        pCached.parse(input, index)
      }
      override def toString = name
    }
    case class Lookahead(p: Parser[_]) extends Parser[Unit]{
      def parse(input: String, index: Int) = {
        p.parse(input, index) match{
          case Success(t, i) => Res.Success((), index)
          case Failure(i, ps) => Failure(i, this :: ps)
        }
      }
    }
    case object Pass extends Parser[Unit]{
      def parse(input: String, index: Int) = Res.Success((), index)
    }
    case object Fail extends Parser[Nothing]{
      def parse(input: String, index: Int) = Failure(index, this :: Nil)
    }
    case class Not(p: Parser[_]) extends Parser[Unit]{
      def parse(input: String, index: Int) = {
        val res0 = p.parse(input, index)
        val res = res0 match{
          case Success(t, i) => Failure(i, List(this))
          case Failure(_, _) => Res.Success((), index)
        }
        res
      }
    }
    case object AnyChar extends Parser[Char]{
      def parse(input: String, index: Int) = {
        if (index >= input.length) Failure(index, List(this))
        else Success(input(index), index+1)
      }
    }
    case class Repeat[+T](p: Parser[T], min: Int, delimiter: Parser[_]) extends Parser[Seq[T]]{
      def parse(input: String, index: Int) = {
        val res = mutable.Buffer.empty[T]
        var finalIndex = index
        var lastFailure: Failure = null
        @tailrec def rec(index: Int, del: Parser[_]): Unit = {
          del.parse(input, index) match{
            case f @ Failure(_, _) => lastFailure = f
            case Success(t, i) =>
              p.parse(input, i) match{
                case f @ Failure(_, _) => lastFailure = f
                case Success(t, i) =>
                  res.append(t)
                  finalIndex = i
                  rec(i, delimiter)
              }
          }

        }
        rec(index, Pass)
        if (res.length >= min) Success(res, finalIndex)
        else Failure(index, List(this))
      }
    }
    case class Either[+T, +V1 <: T, +V2 <: T](p1: Parser[V1], p2: Parser[V2]) extends Parser[T]{
      def parse(input: String, index: Int) = {
        p1.parse(input, index) match{
          case s: Success[_] => s
          case f: Failure => p2.parse(input, index) match{
            case s: Success[_] => s
            case f: Failure => Failure(index, List(this))
          }
        }
      }
    }
    case class Literal(s: String) extends Parser[String]{
      def parse(input: String, index: Int) = {
        if (input.startsWith(s, index)) Res.Success(s, index + s.length)
        else Res.Failure(index, List(this))
      }
    }
    case class CharLiteral(c: Char) extends Parser[Char]{
      def parse(input: String, index: Int) = {
        if (index >= input.length) Res.Failure(index, List(this))
        else if (input(index) == c) Res.Success(c, index + 1)
        else Res.Failure(index, List(this))
      }
    }

    case class CharPredicate(f: Char => Boolean) extends Parser[Char]{
      def parse(input: String, index: Int) = {
        if (index >= input.length) Res.Failure(index, List(this))
        else if (f(input(index))) Res.Success(input(index), index + 1)
        else Res.Failure(index, List(this))
      }
    }
    case class Sequence[+T1, +T2](p1: Parser[T1], p2: Parser[T2]) extends Parser[(T1, T2)]{
      def parse(input: String, index: Int) = {
        p1.parse(input, index) match{
          case s1: Success[_] => p2.parse(input, s1.index) match{
            case s2: Success[_] => Success((s1.t, s2.t), s2.index)
            case f: Failure => f.copy(ps = this :: f.ps)
          }
          case f: Failure => f.copy(ps = this :: f.ps)
        }
      }
    }
//    case class Regex(re: scala.util.matching.Regex) extends Parser[String]{
//      def parse(input: String, index: Int) = {
//
//        if (input.startsWith(s, index)) Res.Success(s, index + s.length)
//        else Res.Failure(index)
//      }
//    }
  }
  implicit def wspStr(s: String) = Parser.Literal(s)
  implicit def wspCh(s: Char) = Parser.CharLiteral(s)

//  implicit def regexToRegex(re: scala.util.matching.Regex): Parser[String] = ???
  def rule[T](p: => Parser[T])(implicit name: FuncName): Parser[T] = Parser.Lazy(name.name, () => p)

  type Rule0 = Parser[_]
}

