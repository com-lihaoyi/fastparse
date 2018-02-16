package fastparse.core
import acyclic.file
import fastparse.core.Implicits._
import fastparse.parsers.Combinators.{Capturing, Cut, Either, Logged, Not, Opaque, Optional, Repeat, Sequence}
import fastparse.parsers.Terminals.Pass
import fastparse.parsers.Transformers.{Filtered, FlatMapped, Mapper}
import fastparse.utils.ReprOps

abstract class ParserApi[Elem, Repr, +T]()(implicit repr: ReprOps[Elem, Repr]) {

  /**
   * Wraps this in a [[Logged]]. This prints out information
   * where a parser was tried and its result, which is useful for debugging
   */
  def log(msg: String = toString)(implicit out: Logger): Parser[Elem, Repr, T]

  /**
   * Makes this parser opaque, i.e. hides it and its inner parsers
   * from the stack trace, providing the specified message instead.
   */
  def opaque(msg: String = toString): Parser[Elem, Repr, T]

  /**
   * Repeats this parser 0 or more times
   */
  def rep[R](implicit ev: Repeater[T, R]): Parser[Elem, Repr, R]
  def rep[R](min: Int = 0,
             sep: Parser[Elem, Repr, _] = Pass[Elem, Repr],
             max: Int = Int.MaxValue,
             exactly: Int = -1)
            (implicit ev: Repeater[T, R]): Parser[Elem, Repr, R]

  /**
   * Parses using this or the parser `p`
   */
  def |[V >: T](p: Parser[Elem, Repr, V]): Parser[Elem, Repr, V]

  /**
   * Parses using this followed by the parser `p`
   */
  def ~[V, R](p: Parser[Elem, Repr, V])
             (implicit ev: Sequencer[T, V, R]): Parser[Elem, Repr, R]

  /**
   * Parses using this followed by the parser `p`, performing a Cut if
   * this parses successfully. That means that if `p` fails to parse, the
   * parse will fail immediately and not backtrack past this success.
   *
   * This lets you greatly narrow the error position by avoiding unwanted
   * backtracking.
   */
  def ~/[V, R](p: Parser[Elem, Repr, V])
              (implicit ev: Sequencer[T, V, R]): Parser[Elem, Repr, R]

  /**
   * Performs a cut if this parses successfully.
   */
  def ~/ : Parser[Elem, Repr, T]

  /**
   * Parses this, optionally
   */
  def ?[R](implicit ev: Optioner[T, R]): Parser[Elem, Repr, R]

  /**
   * Wraps this in a [[Not]] for negative lookahead
   */
  def unary_! : Parser[Elem, Repr, Unit]

  /**
   * Used to capture the text parsed by this as a `String`
   */
  def ! : Parser[Elem, Repr, Repr]

  /**
   * Transforms the result of this Parser with the given function
   */
  def map[V](f: T => V): Parser[Elem, Repr, V]

  /**
   * Uses the result of this parser to create another parser that
   * will be used for the next parse
   */
  def flatMap[V](f: T => Parser[Elem, Repr, V]): Parser[Elem, Repr, V]

  /**
   * applies the supplied predicate to the current parser succeeding
     on true failing on false
   */
  def filter(predicate: T => Boolean): Parser[Elem, Repr, T]

  /**
   * alias for `filter`
   */
  final def withFilter(predicate: T => Boolean): Parser[Elem, Repr, T] = filter(predicate)
}

class ParserApiImpl[Elem, Repr, +T](self: Parser[Elem, Repr, T])
                                       (implicit repr: ReprOps[Elem, Repr])
    extends ParserApi[Elem, Repr, T] {

  def log(msg: String = self.toString)(implicit output: Logger) = Logged(self, msg, output.f)

  def opaque(msg: String = self.toString) = Opaque(self, msg)

  def rep[R](implicit ev: Repeater[T, R]): Parser[Elem, Repr, R] =
    Repeat(self, 0, Int.MaxValue, Pass[Elem, Repr])
  def rep[R](min: Int = 0, sep: Parser[Elem, Repr, _] = Pass[Elem, Repr],
             max: Int = Int.MaxValue, exactly: Int = -1)
            (implicit ev: Repeater[T, R]): Parser[Elem, Repr, R] = {
    if (exactly < 0)
      Repeat(self, min, max, sep)
    else
      Repeat(self, exactly, exactly, sep)
  }

  def |[V >: T](p: Parser[Elem, Repr, V]): Parser[Elem, Repr, V] =
    Either[Elem, Repr, V](Either.flatten(Vector(self, p)):_*)

  def ~[V, R](p: Parser[Elem, Repr, V])(implicit ev: Sequencer[T, V, R]): Parser[Elem, Repr, R] =
    Sequence.flatten(Sequence(self, p, cut=false).asInstanceOf[Sequence[Elem, Repr, R, R, R]])
  def ~/[V, R](p: Parser[Elem, Repr, V])(implicit ev: Sequencer[T, V, R]): Parser[Elem, Repr, R] =
    Sequence.flatten(Sequence(self, p, cut=true).asInstanceOf[Sequence[Elem, Repr, R, R, R]])

  def ?[R](implicit ev: Optioner[T, R]): Parser[Elem, Repr, R] = Optional(self)

  def unary_! : Parser[Elem, Repr, Unit] = Not(self)

  def ~/ : Parser[Elem, Repr, T] = Cut[Elem, Repr, T](self)

  def ! : Parser[Elem, Repr, Repr] = Capturing(self)

  def map[V](f: T => V): Parser[Elem, Repr, V] = Mapper(self, f)

  def flatMap[V](f: T => Parser[Elem, Repr, V]): Parser[Elem, Repr, V] = FlatMapped(self, f)

  def filter(predicate: T => Boolean): Parser[Elem, Repr, T] = Filtered(self,predicate)
}
