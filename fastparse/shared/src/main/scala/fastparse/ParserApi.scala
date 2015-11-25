package fastparse
import acyclic.file
import parsers.Terminals._
import parsers.Combinators._
import parsers.Transformers._
import Implicits._
import core.Parser

trait ParserApi[+T] {

  /**
   * Wraps this in a [[Logged]]. This prints out information
   * where a parser was tried and its result, which is useful for debugging
   */
  def log(msg: String = this.toString)(implicit output: Logger): Parser[T]

  /**
   * Makes this parser opaque, i.e. hides it and its inner parsers
   * from the stack trace, providing the specified message instead.
   */
  def opaque(msg: String = this.toString): Parser[T]

  /**
   * Repeats this parser 0 or more times
   */
  def rep[R](implicit ev: Repeater[T, R]): Parser[R]
  def rep[R](min: Int = 0,
             sep: Parser[_] = Pass,
             max: Int = Int.MaxValue)
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
  def ~/[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R]

  /**
   * Performs a cut if this parses successfully.
   */
  def ~/ : Parser[T]
  /**
   * Parses this, optionally
   */
  def ?[R](implicit ev: Optioner[T, R]): Parser[R]

  /**
   * Wraps this in a [[Not]] for negative lookaheak
   */
  def unary_! : Parser[Unit]

  /**
   * Used to capture the text parsed by this as a `String`
   */
  def ! : Parser[String]

  /**
   * Transforms the result of this Parser with the given function
   */
  def map[V](f: T => V): Parser[V]
  /**
   * Uses the result of this parser to create another parser that
   * will be used for the next parse
   */
  def flatMap[V](f: T => Parser[V]): Parser[V]

  /**
   * applies the supplied predicate to the current parser succeeding on true failing on false
   */
  def filter(predicate: T => Boolean): Parser[T]
}

class ParserApiImpl[+T](self: Parser[T]) extends ParserApi[T] {

  def log(msg: String = self.toString)(implicit output: Logger) = Logged(self, msg, output.f)

  def opaque(msg: String = self.toString) = Opaque(self, msg)

  def rep[R](implicit ev: Repeater[T, R]): Parser[R] = Repeat(self, 0, Int.MaxValue, Pass)
  def rep[R](min: Int = 0, sep: Parser[_] = Pass, max: Int = Int.MaxValue)
            (implicit ev: Repeater[T, R]): Parser[R] = Repeat(self, min, max, sep)

  def |[V >: T](p: Parser[V]): Parser[V] = Either[V](Either.flatten(Vector(self, p)):_*)

  def ~[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] =
    Sequence.flatten(Sequence(self, p, cut=false).asInstanceOf[Sequence[R, R, R]])
  def ~/[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] =
    Sequence.flatten(Sequence(self, p, cut=true).asInstanceOf[Sequence[R, R, R]])

  def ?[R](implicit ev: Optioner[T, R]): Parser[R] = Optional(self)

  def unary_! : Parser[Unit] = Not(self)

  def ~/ : Parser[T] = Cut[T](self)

  def ! : Parser[String] = Capturing(self)

  def map[V](f: T => V): Parser[V] = Mapper(self, f)

  def flatMap[V](f: T => Parser[V]): Parser[V] = FlatMapped(self, f)

  def filter(predicate: T => Boolean): Parser[T] = Filtered(self,predicate)
}
