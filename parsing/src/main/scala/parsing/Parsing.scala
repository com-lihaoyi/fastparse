package parsing

import scala.annotation.{switch, tailrec}
import scala.collection.{BitSet, mutable}
import acyclic.file
import Utils._
/**
 * Result of a parse, whether successful or failed
 */
sealed trait Result[+T]

object Result{
  case class Frame(index: Int, parser: Parser[_])
  /**
   * @param value The result of this parse
   * @param index The index where the parse completed; may be less than
   *              the length of input
   * @param cut Whether or not this parse encountered a Cut
   */
  case class Success[T](value: T, index: Int, cut: Boolean = false) extends Result[T]

  /**
   * @param input The input string for the failed parse. Useful so the [[Failure]]
   *              object can pretty-print snippet
   * @param fullStack The entire stack trace where the parse failed, containing every
   *                  parser in the stack and the index where the parser was used
   * @param index The index in the parse where this parse failed
   * @param parser The deepest parser in the parse which failed
   * @param cut Whether or not this parse encountered a Cut
   */
  case class Failure(input: String,
                     fullStack: List[Frame],
                     index: Int,
                     parser: Parser[_],
                     cut: Boolean) extends Result[Nothing]{
    /**
     * A slimmed down version of [[fullStack]], this only includes named
     * [[Parser.Rule]] objects as well as the final Parser (whether named or not)
     * for easier reading.
     */
    def stack = fullStack.filter(_.parser.isInstanceOf[Parser.Rule[_]]) :+ Frame(index, parser)

    /**
     * A longer version of [[trace]], which shows more context for every stack frame
     */
    def verboseTrace = {
      val body =
        for(Frame(index, p) <- stack)
          yield s"$index\t...${literalize(input.slice(index, index+5))}\t$p"
      body.mkString("\n")
    }

    /**
     * A one-line snippet that tells you what the state of the parser was when it failed
     */
    def trace = {
      val body =
        for(Frame(index, p) <- stack)
          yield s"$p:$index"

      body.mkString(" / ") + " ..." + literalize(input.slice(index, index+10))
    }
    override def toString = s"Failure($trace, $cut)"
  }
}
import Result._

/**
 * A single, self-contained, immutable parser. The primary method is
 * `parse`, which returns a [[T]] on success and a stack trace on failure.
 */
sealed trait Parser[+T]{

  /**
    * Parses the given `input` starting from the given `index`
   */
  def parse(input: String, index: Int = 0, trace: Boolean = true): Result[T] = parseRecursive(input, index, 0, trace)
  /**
    * Parses the given `input` starting from the given `index` and `logDepth`
   */
  def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean): Result[T]

  /**
   * Wraps this in a [[Parser.Logged]]. This prints out information where a parser
   * was tried and its result, which is useful for debugging
   */
  def log(msg: String, output: String => Unit = println) = Parser.Logged(this, msg, output)
  /**
   * Repeats this parser 0 or more times
   */
  def rep[R](implicit ev: Implicits.Repeater[T, R]): Parser[R] = Parser.Repeat(this, 0, Parser.Pass)
  /**
   * Repeats this parser 1 or more times
   */
  def rep1[R](implicit ev: Implicits.Repeater[T, R]): Parser[R] = Parser.Repeat(this, 1, Parser.Pass)
  /**
   * Repeats this parser 0 or more times, with a delimiter
   */
  def rep[R](delimiter: Parser[_])(implicit ev: Implicits.Repeater[T, R]): Parser[R] = Parser.Repeat(this, 0, delimiter)
  /**
   * Repeats this parser 1 or more times, with a delimiter
   */
  def rep1[R](delimiter: Parser[_])(implicit ev: Implicits.Repeater[T, R]): Parser[R] = Parser.Repeat(this, 1, delimiter)

  /**
   * Parses using this or the parser `p`
   */
  def |[V >: T](p: Parser[V]): Parser[V] = Parser.Either[V](this, p)

  /**
   * Parses using this followed by the parser `p`
   */
  def ~[V, R](p: Parser[V])(implicit ev: Implicits.Sequencer[T, V, R]): Parser[R] = Parser.Sequence(this, p, cut=false)
  /**
   * Parses using this followed by the parser `p`, performing a Cut if
   * this parses successfully. That means that if `p` fails to parse, the
   * parse will fail immediately and not backtrack past this success.
   *
   * This lets you greatly narrow the error position by avoiding unwanted
   * backtracking.
   */
  def ~![V, R](p: Parser[V])(implicit ev: Implicits.Sequencer[T, V, R]): Parser[R] = Parser.Sequence(this, p, cut=true)

  /**
   * Parses this, optionally
   */
  def ?[R](implicit ev: Implicits.Optioner[T, R]) = Parser.Optional(this)

  /**
   * Wraps this in a [[Parser.Not]] for negative lookaheal
   */
  def unary_! = Parser.Not(this)

  /**
   * Used to capture the text parsed by this as a `String`
   */
  def ! = Parser.Capturing(this)

  /**
   * Transforms the result of this Parser with the given function
   */
  def map[V](f: T => V): Parser[V] = Parser.Mapper(this, f)

  protected def fail(input: String, index: Int) =
    Failure(input, Nil, index, this, cut=false)
  protected def failMore(f: Failure, index: Int, trace: Boolean, cut: Boolean = false) = {
    val newStack = if (!trace) f.fullStack else new ::(new Result.Frame(index, this), f.fullStack)
    Failure(f.input, newStack, f.index, f.parser, cut = f.cut || cut)
  }
}

object Parser{

  /**
   * Applies a transformation [[f]] to the result of [[p]]
   */
  case class Mapper[T, V](p: Parser[T], f: T => V) extends Parser[V]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      p.parseRecursive(input, index, logDepth, trace) match{
        case s: Success[T] => Success(f(s.value), s.index, s.cut)
        case f: Failure => failMore(f, index, trace)
      }
    }
  }
  /**
   * A parser that always succeeds, consuming no input
   */
  case object Pass extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = Result.Success((), index)
  }

  /**
   * A parser that always fails immediately
   */
  case object Fail extends Parser[Nothing]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = fail(input, index)
  }

  /**
   * Captures the string parsed by the given parser [[p]].
   */
  case class Capturing(p: Parser[_]) extends Parser[String]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      p.parseRecursive(input, index, logDepth, trace) match {
        case s: Success[_] => Success(input.substring(index, s.index), s.index, s.cut)
        case f: Failure => f
      }
    }
    override def toString = p.toString + ".!"
  }
  /**
   * Succeeds, consuming a single character
   */
  case object AnyChar extends Parser[Char]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      if (index >= input.length) fail(input, index)
      else Success(input(index), index+1)
    }
  }

  /**
   * Succeeds if at the start of the input, consuming no input
   */
  case object Start extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      if (index == 0) Success((), index)
      else fail(input, index)
    }
  }
  /**
   * Succeeds if at the end of the input, consuming no input
   */
  case object End extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      if (index == input.length) Success((), index)
      else fail(input, index)
    }
  }

  /**
   * Parses a literal `String`
   */
  case class Literal(s: String) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      if (input.startsWith(s, index)) Result.Success(s, index + s.length)
      else fail(input, index)
    }
    override def toString = literalize(s).toString
  }

  /**
   * Parses a single character
   */
  case class CharLiteral(c: Char) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      if (index >= input.length) fail(input, index)
      else if (input(index) == c) Result.Success(c.toString, index + 1)
      else fail(input, index)
    }
    override def toString = literalize(c.toString).toString
  }


  /**
   * Wraps a parser and prints out the indices where it starts
   * and ends, together with its result
   */
  case class Logged[+T](p: Parser[T], msg: String, output: String => Unit) extends Parser[T]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      val indent = "  " * logDepth
      output(indent + "+" + msg + ":" + index)
      val res = p.parseRecursive(input, index, logDepth + 1, trace)
      output(indent + "-" + msg + ":" + index + ":" + res)
      res
    }
  }


  /**
   * A top-level, named parser. Lazily evaluates the wrapped parser
   * [[p]] only when `parse` is called to allow for circular
   * dependencies between parsers.
   */
  case class Rule[+T](name: String, p: () => Parser[T]) extends Parser[T]{
    lazy val pCached = p()
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      pCached.parseRecursive(input, index, logDepth, trace) match{
        case f: Failure => failMore(f, index, trace)
        case s => s
      }
    }
    override def toString = name
  }

  /**
   * Wraps another parser, succeeding/failing identically
   * but consuming no input
   */
  case class Lookahead(p: Parser[_]) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      p.parseRecursive(input, index, logDepth, trace) match{
        case s: Success[_] => Result.Success((), index)
        case f: Failure => failMore(f, index, trace)
      }
    }
    override def toString = s"&($p)"
  }
  /**
   * Wraps another parser, succeeding it it fails and failing
   * if it succeeds. Neither case consumes any input
   */
  case class Not(p: Parser[_]) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      val res0 = p.parseRecursive(input, index, logDepth, trace)
      val res = res0 match{
        case s: Success[_] => fail(input, s.index)
        case f: Failure => Result.Success((), index)
      }
      res
    }
    override def toString = s"!($p)"
  }


  /**
   * Wraps a parser and succeeds with `Some` if [[p]] succeeds,
   * and succeeds with `None` if [[p]] fails.
   */
    case class Optional[+T, R](p: Parser[T])
                              (implicit ev: Implicits.Optioner[T, R]) extends Parser[R]{

    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      p.parseRecursive(input, index, logDepth, trace) match{
        case Success(t, index, cut) => Success(ev(Some(t)), index, cut)
        case f: Failure if f.cut => failMore(f, index, trace)
        case _ => Success(ev(None), index)
      }
    }
    override def toString = s"$p.?"
  }

  /**
   * Parsers two things in a row, returning a tuple of the two
   * results if both things succeed
   */
  case class Sequence[+T1, +T2, R](p1: Parser[T1], p2: Parser[T2], cut: Boolean)
                                  (implicit ev: Implicits.Sequencer[T1, T2, R]) extends Parser[R]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      p1.parseRecursive(input, index, logDepth, trace) match{
        case f: Failure => failMore(f, index, trace, cut = f.cut)
        case s1: Success[_] =>
          p2.parseRecursive(input, s1.index, logDepth, trace) match{
          case f: Failure => failMore(f, index, trace, cut = cut || f.cut || s1.cut)
          case s2: Success[_] => Success(ev(s1.value, s2.value), s2.index, s2.cut || s1.cut | cut)
        }
      }
    }

    override def toString = {
      def rec(p: Parser[_]): String = p match {
        case p: Sequence[_, _, _] =>
          val op = if(cut) "~!" else "~"
          rec(p.p1) + " " + op + " " + rec(p.p2)
        case p => p.toString
      }
      "(" + rec(this) + ")"
    }
  }

  /**
   * Repeats the parser over and over. Succeeds with a `Seq` of results
   * if there are more than [[min]] successful parses. uses the [[delimiter]]
   * between parses and discards its results
   */
  case class Repeat[T, +R](p: Parser[T], min: Int, delimiter: Parser[_])
                          (implicit ev: Implicits.Repeater[T, R]) extends Parser[R]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      val res = mutable.Buffer.empty[T]
      var finalIndex = index
      var lastFailure: Failure = null
      @tailrec def rec(index: Int, del: Parser[_]): Unit = {
        del.parseRecursive(input, index, logDepth, trace) match{
          case f: Failure if f.cut => lastFailure = failMore(f, index, trace)
          case f: Failure => lastFailure = f
          case Success(t, i, cut1) =>
            p.parseRecursive(input, i, logDepth, trace) match{
              case f: Failure if f.cut | cut1 => lastFailure = failMore(f, index, f.cut | cut1)
              case f: Failure => lastFailure = f
              case Success(t, i, cut2) =>
                res.append(t)
                finalIndex = i
                rec(i, delimiter)
            }
        }
      }
      rec(index, Pass)
      if (lastFailure != null && lastFailure.cut) {
        failMore(lastFailure, index, trace)
      }
      else if (res.length >= min) Success(ev(res.iterator), finalIndex)
      else fail(input, index)
    }
    override def toString = {
      p + ".rep" + (if (min == 0) "" else min) + (if (delimiter == Pass) "" else s"($delimiter)")
    }
  }

  object Either{
    def flatten[T](p: Parser[T]) = p match{
      case Either(p1, p2) =>
      case p => p
    }
  }
  /**
   * Parses using one parser or the other, if the first one fails. Returns
   * the first one that succeeds and fails if both fail
   */
  case class Either[T](ps: Parser[T]*) extends Parser[T]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      @tailrec def rec(parserIndex: Int): Result[T] = {
        if (parserIndex >= ps.length) fail(input, index)
        else ps(parserIndex).parseRecursive(input, index, logDepth, trace) match {
          case s: Success[_] => s
          case f: Failure if f.cut => failMore(f, index, trace)
          case _ => rec(parserIndex + 1)
        }
      }
      rec(0)
    }
    override def toString = {
      def rec(p: Parser[_]): String = p match {
        case p: Either[_] => p.ps.map(rec).mkString(" | ")
        case p => p.toString
      }
      "(" + rec(this) + ")"
    }
  }

  /**
   * Parses a single character if it passes the predicate
   */
  case class CharPred(predicate: Char => Boolean) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      if (index >= input.length) fail(input, index)
      else if (predicate(input(index))) Result.Success(input(index), index + 1)
      else fail(input, index)
    }
  }
  /**
   * Parses a single character if it passes the predicate
   */
  case class CharIn(strings: Seq[Char]*) extends Parser[Unit]{
    private[this] val uberSet = BitSet(strings.flatten.map(_.toInt):_*)
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      if (index >= input.length) fail(input, index)
      else if (uberSet(input(index))) Result.Success(input(index), index + 1)
      else fail(input, index)
    }
    override def toString = {
      s"CharIn(${literalize(strings.flatten.mkString)})"
    }
  }

  /**
   * Very efficiently attempts to parse a set of strings, by
   * first converting it into a Trie and then walking it once.
   * If multiple strings match the input, longest match wins.
   */
  case class StringIn(strings: String*) extends Parser[Unit]{
    private[this ]case class Node(children: mutable.LongMap[Node] = mutable.LongMap.empty,
                                  var word: String = null)
    private[this] val bitSet = Node()
    for(string <- strings){
      var current = bitSet
      for(char <- string){
        val next = current.children.getOrNull(char)
        if (next == null) {
          current.children(char) = Node()
        }
        current = current.children(char)
      }
      current.word = string
    }
    def parseRecursive(input: String, index: Int, logDepth: Int, trace: Boolean) = {
      @tailrec def rec(offset: Int, currentNode: Node, currentRes: Result[Unit]): Result[Unit] = {
        if (index + offset >= input.length) currentRes
        else {
          val char = input(index + offset)
          val next = currentNode.children.getOrNull(char)
          if (next == null) currentRes
          else {
            rec(
              offset + 1,
              next,
              if (next.word != null) Success((), index + offset + 1) else currentRes
            )
          }
        }
      }
      rec(0, bitSet, fail(input, index))
    }
    override def toString = {
      s"StringIn(${strings.map(literalize(_)).mkString(", ")})"
    }
  }
}

