package fastparse
import acyclic.file
import parsers.Terminals._
import parsers.Combinators._
import parsers.Transformers._
import Implicits._
import core.Parser

trait ParserApi[+T, ElemType, Repr] {

  /**
   * Wraps this in a [[Logged]]. This prints out information
   * where a parser was tried and its result, which is useful for debugging
   */
  def log(msg: String = this.toString)(implicit output: Logger): Parser[T, ElemType, Repr]

  /**
   * Makes this parser opaque, i.e. hides it and its inner parsers
   * from the stack trace, providing the specified message instead.
   */
  def opaque(msg: String = this.toString): Parser[T, ElemType, Repr]

  /**
   * Repeats this parser 0 or more times
   */
  def rep[R](implicit ev: Repeater[T, R]): Parser[R, ElemType, Repr]
  def rep[R](min: Int = 0,
             sep: Parser[_, ElemType, Repr] = Pass[ElemType, Repr],
             max: Int = Int.MaxValue,
             exactly: Int = -1)
            (implicit ev: Repeater[T, R]): Parser[R, ElemType, Repr]

  /**
   * Parses using this or the parser `p`
   */
  def |[V >: T](p: Parser[V, ElemType, Repr]): Parser[V, ElemType, Repr]

  /**
   * Parses using this followed by the parser `p`
   */
  def ~[V, R](p: Parser[V, ElemType, Repr])(implicit ev: Sequencer[T, V, R]): Parser[R, ElemType, Repr]
  /**
   * Parses using this followed by the parser `p`, performing a Cut if
   * this parses successfully. That means that if `p` fails to parse, the
   * parse will fail immediately and not backtrack past this success.
   *
   * This lets you greatly narrow the error position by avoiding unwanted
   * backtracking.
   */
  def ~/[V, R](p: Parser[V, ElemType, Repr])(implicit ev: Sequencer[T, V, R]): Parser[R, ElemType, Repr]

  /**
   * Performs a cut if this parses successfully.
   */
  def ~/ : Parser[T, ElemType, Repr]
  /**
   * Parses this, optionally
   */
  def ?[R](implicit ev: Optioner[T, R]): Parser[R, ElemType, Repr]

  /**
   * Wraps this in a [[Not]] for negative lookaheak
   */
  def unary_! : Parser[Unit, ElemType, Repr]

  /**
   * Used to capture the text parsed by this as a `String`
   */
  def ! : Parser[Repr, ElemType, Repr]

  /**
   * Transforms the result of this Parser with the given function
   */
  def map[V](f: T => V): Parser[V, ElemType, Repr]
  /**
   * Uses the result of this parser to create another parser that
   * will be used for the next parse
   */
  def flatMap[V](f: T => Parser[V, ElemType, Repr]): Parser[V, ElemType, Repr]

  /**
   * applies the supplied predicate to the current parser succeeding on true failing on false
   */
  def filter(predicate: T => Boolean): Parser[T, ElemType, Repr]
}

class ParserApiImpl[+T, ElemType, Repr](self: Parser[T, ElemType, Repr])
                                       (implicit builder: ResultConverter[ElemType, Repr])
    extends ParserApi[T, ElemType, Repr] {

  def log(msg: String = self.toString)(implicit output: Logger) = Logged(self, msg, output.f)

  def opaque(msg: String = self.toString) = Opaque(self, msg)

  def rep[R](implicit ev: Repeater[T, R]): Parser[R, ElemType, Repr] =
    Repeat(self, 0, Int.MaxValue, Pass[ElemType, Repr])
  def rep[R](min: Int = 0, sep: Parser[_, ElemType, Repr] = Pass[ElemType, Repr],
             max: Int = Int.MaxValue, exactly: Int = -1)
            (implicit ev: Repeater[T, R]): Parser[R, ElemType, Repr] = {
    if (exactly < 0)
      Repeat(self, min, max, sep)
    else
      Repeat(self, exactly, exactly, sep)
  }

  def |[V >: T](p: Parser[V, ElemType, Repr]): Parser[V, ElemType, Repr] =
    Either[V, ElemType, Repr](Either.flatten(Vector(self, p)):_*)

  def ~[V, R](p: Parser[V, ElemType, Repr])(implicit ev: Sequencer[T, V, R]): Parser[R, ElemType, Repr] =
    Sequence.flatten(Sequence(self, p, cut=false).asInstanceOf[Sequence[R, R, R, ElemType, Repr]])
  def ~/[V, R](p: Parser[V, ElemType, Repr])(implicit ev: Sequencer[T, V, R]): Parser[R, ElemType, Repr] =
    Sequence.flatten(Sequence(self, p, cut=true).asInstanceOf[Sequence[R, R, R, ElemType, Repr]])

  def ?[R](implicit ev: Optioner[T, R]): Parser[R, ElemType, Repr] = Optional(self)

  def unary_! : Parser[Unit, ElemType, Repr] = Not(self)

  def ~/ : Parser[T, ElemType, Repr] = Cut[T, ElemType, Repr](self)

  def ! : Parser[Repr, ElemType, Repr] = Capturing(self)

  def map[V](f: T => V): Parser[V, ElemType, Repr] = Mapper(self, f)

  def flatMap[V](f: T => Parser[V, ElemType, Repr]): Parser[V, ElemType, Repr] = FlatMapped(self, f)

  def filter(predicate: T => Boolean): Parser[T, ElemType, Repr] = Filtered(self,predicate)
}
