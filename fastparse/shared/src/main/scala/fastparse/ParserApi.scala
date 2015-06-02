package fastparse

import fastparse.core.{ParseCtx, Result}
import parsers.Combinators._
import parsers.Transformers._
import Implicits._
trait Parser[+T] extends core.Parser[T] with ParserApi[T] with ParserApiImpl[T]
trait ParserApi[+T]{
  import Implicits._

  /**
   * Wraps this in a [[Logged]]. This prints out information
   * where a parser was tried and its result, which is useful for debugging
   */
  def log(msg: String)(implicit output: Logger): Parser[T]
  /**
   * Repeats this parser 0 or more times
   */
  def rep[R](implicit ev: Repeater[T, R]): Parser[R]
  def rep[R](min: Int = 0,
             sep: Parser[_] = Pass,
             until: Parser[_] = Pass)
            (implicit ev: Repeater[T, R]): Parser[R]

  /**
   * Parses using this or the parser `p`
   */
  def |[V >: T](p: Parser[V]): Parser[V]

  /**
   * Parses using this followed by the parser `p`
   */
  def ~[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R]
  /**
   * Parses using this followed by the parser `p`, performing a Cut if
   * this parses successfully. That means that if `p` fails to parse, the
   * parse will fail immediately and not backtrack past this success.
   *
   * This lets you greatly narrow the error position by avoiding unwanted
   * backtracking.
   */
  def ~![V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R]

  /**
   * Performs a cut if this parses successfully.
   */
  def ~! : Parser[T]
  /**
   * Parses this, optionally
   */
  def ?[R](implicit ev: Optioner[T, R]): Parser[R]

  /**
   * Wraps this in a [[Not]] for negative lookaheak
   */
  def unary_! : Parser[_]

  /**
   * Used to capture the text parsed by this as a `String`
   */
  def ! : Parser[_]

  /**
   * Transforms the result of this Parser with the given function
   */
  def map[V](f: T => V): Parser[V]
  /**
   * Uses the result of this parser to create another parser that
   * will be used for the next parse
   */
  def flatMap[V](f: T => Parser[V]): Parser[V]
}
import parsers.Terminals.Pass
trait ParserApiImpl[+T] extends ParserApi[T]{ this: Parser[T] =>

  def parseRec(cfg: ParseCtx, index: Int): Result[T]

  def log(msg: String)(implicit output: Logger) = Logged(this, msg, output.f)

  def rep[R](implicit ev: Repeater[T, R]): Parser[R] = Repeat(this, 0, Pass, Pass)
  def rep[R](min: Int = 0, sep: Parser[_] = Pass, end: Parser[_] = Pass)
            (implicit ev: Repeater[T, R]): Parser[R] = Repeat(this, min, sep, end)

  def |[V >: T](p: Parser[V]): Parser[V] = Either[V](Either.flatten(Vector(this, p)):_*)

  def ~[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] =
    Sequence.flatten(Sequence(this, p, cut=false).asInstanceOf[Sequence[R, R, R]])
  def ~![V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] =
    Sequence.flatten(Sequence(this, p, cut=true).asInstanceOf[Sequence[R, R, R]])

  def ?[R](implicit ev: Optioner[T, R]) = Optional(this)

  def unary_! = Not(this)

  def ~! : Parser[T] = Cut[T](this)

  def ! = Capturing(this)

  def map[V](f: T => V): Parser[V] = Mapper(this, f)

  def flatMap[V](f: T => Parser[V]): Parser[V] = FlatMapped(this, f)

  def filter(predicate: T => Boolean): Parser[T] = Filtered(this,predicate)
}