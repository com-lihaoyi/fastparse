package fastparse.core
import acyclic.file
import fastparse.core.Implicits._
import fastparse.parsers.Combinators.{Capturing, Cut, Either, Logged, Not, Opaque, Optional, Repeat, Sequence}
import fastparse.parsers.Terminals.Pass
import fastparse.parsers.Transformers.{Filtered, FlatMapped, Mapper}
import fastparse.utils.ReprOps

abstract class ParserApi[+T, Elem, Repr]()(implicit repr: ReprOps[Elem, Repr]) {

  /**
   * Wraps this in a [[Logged]]. This prints out information
   * where a parser was tried and its result, which is useful for debugging
   */
  def log(msg: String = toString)(implicit out: Logger): Parser[T, Elem, Repr]

  /**
   * Makes this parser opaque, i.e. hides it and its inner parsers
   * from the stack trace, providing the specified message instead.
   */
  def opaque(msg: String = toString): Parser[T, Elem, Repr]

  /**
   * Repeats this parser 0 or more times
   */
  def rep[R](implicit ev: Repeater[T, R]): Parser[R, Elem, Repr]
  def rep[R](min: Int = 0,
             sep: Parser[_, Elem, Repr] = Pass[Elem, Repr],
             max: Int = Int.MaxValue,
             exactly: Int = -1)
            (implicit ev: Repeater[T, R]): Parser[R, Elem, Repr]

  /**
   * Parses using this or the parser `p`
   */
  def |[V >: T](p: Parser[V, Elem, Repr]): Parser[V, Elem, Repr]

  /**
   * Parses using this followed by the parser `p`
   */
  def ~[V, R](p: Parser[V, Elem, Repr])
             (implicit ev: Sequencer[T, V, R]): Parser[R, Elem, Repr]

  /**
   * Parses using this followed by the parser `p`, performing a Cut if
   * this parses successfully. That means that if `p` fails to parse, the
   * parse will fail immediately and not backtrack past this success.
   *
   * This lets you greatly narrow the error position by avoiding unwanted
   * backtracking.
   */
  def ~/[V, R](p: Parser[V, Elem, Repr])
              (implicit ev: Sequencer[T, V, R]): Parser[R, Elem, Repr]

  /**
   * Performs a cut if this parses successfully.
   */
  def ~/ : Parser[T, Elem, Repr]

  /**
   * Parses this, optionally
   */
  def ?[R](implicit ev: Optioner[T, R]): Parser[R, Elem, Repr]

  /**
   * Wraps this in a [[Not]] for negative lookaheak
   */
  def unary_! : Parser[Unit, Elem, Repr]

  /**
   * Used to capture the text parsed by this as a `String`
   */
  def ! : Parser[Repr, Elem, Repr]

  /**
   * Transforms the result of this Parser with the given function
   */
  def map[V](f: T => V): Parser[V, Elem, Repr]

  /**
   * Uses the result of this parser to create another parser that
   * will be used for the next parse
   */
  def flatMap[V](f: T => Parser[V, Elem, Repr]): Parser[V, Elem, Repr]

  /**
   * applies the supplied predicate to the current parser succeeding
     on true failing on false
   */
  def filter(predicate: T => Boolean): Parser[T, Elem, Repr]

  /**
   * alias for `filter`
   */
  final def withFilter(predicate: T => Boolean): Parser[T, Elem, Repr] = filter(predicate)
}

class ParserApiImpl[+T, Elem, Repr](self: Parser[T, Elem, Repr])
                                       (implicit repr: ReprOps[Elem, Repr])
    extends ParserApi[T, Elem, Repr] {

  def log(msg: String = self.toString)(implicit output: Logger) = Logged(self, msg, output.f)

  def opaque(msg: String = self.toString) = Opaque(self, msg)

  def rep[R](implicit ev: Repeater[T, R]): Parser[R, Elem, Repr] =
    Repeat(self, 0, Int.MaxValue, Pass[Elem, Repr])
  def rep[R](min: Int = 0, sep: Parser[_, Elem, Repr] = Pass[Elem, Repr],
             max: Int = Int.MaxValue, exactly: Int = -1)
            (implicit ev: Repeater[T, R]): Parser[R, Elem, Repr] = {
    if (exactly < 0)
      Repeat(self, min, max, sep)
    else
      Repeat(self, exactly, exactly, sep)
  }

  def |[V >: T](p: Parser[V, Elem, Repr]): Parser[V, Elem, Repr] =
    Either[V, Elem, Repr](Either.flatten(Vector(self, p)):_*)

  def ~[V, R](p: Parser[V, Elem, Repr])(implicit ev: Sequencer[T, V, R]): Parser[R, Elem, Repr] =
    Sequence.flatten(Sequence(self, p, cut=false).asInstanceOf[Sequence[R, R, R, Elem, Repr]])
  def ~/[V, R](p: Parser[V, Elem, Repr])(implicit ev: Sequencer[T, V, R]): Parser[R, Elem, Repr] =
    Sequence.flatten(Sequence(self, p, cut=true).asInstanceOf[Sequence[R, R, R, Elem, Repr]])

  def ?[R](implicit ev: Optioner[T, R]): Parser[R, Elem, Repr] = Optional(self)

  def unary_! : Parser[Unit, Elem, Repr] = Not(self)

  def ~/ : Parser[T, Elem, Repr] = Cut[T, Elem, Repr](self)

  def ! : Parser[Repr, Elem, Repr] = Capturing(self)

  def map[V](f: T => V): Parser[V, Elem, Repr] = Mapper(self, f)

  def flatMap[V](f: T => Parser[V, Elem, Repr]): Parser[V, Elem, Repr] = FlatMapped(self, f)

  def filter(predicate: T => Boolean): Parser[T, Elem, Repr] = Filtered(self,predicate)
}
