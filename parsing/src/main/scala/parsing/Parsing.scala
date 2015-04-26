package parsing

import parsing.Parsing.Res.Success

import scala.annotation.tailrec
import scala.collection.mutable

object Parsing {
  sealed trait Res[+T]
  object Res{
    case class Success[T](t: T, index: Int) extends Res[T]
    case class Failure(index: Int) extends Res[Nothing]
  }
  import Res._
  sealed trait Parser[T]{
    def parse(input: String, index: Int): Res[T]
    def rep = Parser.Repeat(this, 0)
    def rep1 = Parser.Repeat(this, 1)
    def |[T1 >: T, V <: T1](p: Parser[V]) = Parser.Either(this, p)
    def ~[V](p: Parser[V]) = Parser.Sequence(this, p)
  }
  object Parser{
    case class Repeat[T](p: Parser[T], min: Int) extends Parser[Seq[T]]{
      def parse(input: String, index: Int) = {
        val res = mutable.Buffer.empty[T]
        var finalIndex = index
        @tailrec def rec(index: Int): Unit = {
          p.parse(input, index) match{
            case Failure(_) =>
            case Success(t, i) =>
              res.append(t)
              finalIndex = i
              rec(i)
          }
        }
        rec(index)
        if (res.length >= min) Success(res, finalIndex)
        else Failure(index)
      }
    }
    case class Either[T, V1 <: T, V2 <: T](p1: Parser[V1], p2: Parser[V2]) extends Parser[T]{
      def parse(input: String, index: Int) = {
        p1.parse(input, index) match{
          case s: Success[_] => s
          case f: Failure => p2.parse(input, index) match{
            case s: Success[_] => s
            case f: Failure => Failure(index)
          }
        }
      }
    }
    case class Literal(s: String) extends Parser[String]{
      def parse(input: String, index: Int) = {
        if (input.startsWith(s, index)) Res.Success(s, index + s.length)
        else Res.Failure(index)
      }
    }
    case class Sequence[T1, T2](p1: Parser[T1], p2: Parser[T2]) extends Parser[(T1, T2)]{
      def parse(input: String, index: Int) = {
        p1.parse(input, index) match{
          case s1: Success[_] => p2.parse(input, s1.index) match{
            case s2: Success[_] => Success((s1.t, s2.t), s2.index)
            case f: Failure => f
          }
          case f: Failure => f
        }
      }
    }
//    case class Regex(re: scala.util.matching.Regex) extends Parser[String]
  }
  implicit val stringToLiteral = Parser.Literal
//  implicit val regexToRegex = Parser.Regex
  def rule[T](p: Parser[T]): Parser[T] = p
  def main(args: Array[String]): Unit = {
    println(23)
  }
}

