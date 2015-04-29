package parsing

import scala.annotation.{switch, tailrec}
import scala.collection.{BitSet, mutable}
import acyclic.file

/**
 * Result of a parse, whether successful or failed
 */
sealed trait Result[+T]
object Result{
  case class Success[T](t: T, index: Int, cut: Boolean = false) extends Result[T]{
    override def toString = {
      s"Success($index, $cut)"
    }
  }

  /**
   * Convert a string to a C&P-able literal. Basically
   * copied verbatim from the uPickle source code.
   */
  def literalize(s: String, unicode: Boolean = true) = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"')

  }
  case class Failure(input: String, fullStack: List[(Int, Parser[_])], cut: Boolean) extends Result[Nothing]{
    def stack = fullStack.filter(_._2.isInstanceOf[Parser.Rule[_]]) :+ fullStack.last

    def verboseTrace = {
      val body =
        for((index, p) <- stack)
          yield s"$index\t...${literalize(input.slice(index, index+5))}\t$p"
      body.mkString("\n")
    }
    def trace = {
      val body =
        for((index, p) <- stack)
          yield s"$p:$index"
      val lastIndex = fullStack.last._1
      body.mkString(" / ") + " ..." + literalize(input.slice(lastIndex, lastIndex+10))
    }
    override def toString = s"Failure($trace, $cut)"
  }
}
import Result._
sealed trait Parser[+T]{
  /**
   * Wraps this in a [[Parser.Logged]]
   */
  def log(msg: String) = Parser.Logged(this, msg)

  /**
   * Parses the given `input` starting from the given `index`
   */
  def parse(input: String, index: Int = 0): Result[T] = parseRecursive(input, index, 0)
  /**
   * Parses the given `input` starting from the given `index` and `logDepth`
   */
  def parseRecursive(input: String, index: Int, logDepth: Int): Result[T]

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
    Failure(input, (index -> this) :: Nil, cut=false)
  protected def failMore(f: Failure, index: Int, cut: Boolean = false) =
    Failure(f.input, (index -> this) :: f.fullStack, cut=f.cut || cut)
}

object Parser{
  case class Mapper[T, V](p: Parser[T], f: T => V) extends Parser[V]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      p.parseRecursive(input, index, logDepth) match{
        case s: Success[T] => Success(f(s.t), s.index, s.cut)
        case f: Failure => failMore(f, index)
      }
    }
  }
  /**
   * A parser that always succeeds, consuming no input
   */
  case object Pass extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = Result.Success((), index)
  }

  /**
   * A parser that always fails immediately
   */
  case object Fail extends Parser[Nothing]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = fail(input, index)
  }

  case class Capturing(p: Parser[_]) extends Parser[String]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      p.parseRecursive(input, index, logDepth) match {
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
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      if (index >= input.length) fail(input, index)
      else Success(input(index), index+1)
    }
  }

  /**
   * Succeeds if at the start of the input, consuming no input
   */
  case object Start extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      if (index == 0) Success((), index)
      else fail(input, index)
    }
  }
  /**
   * Succeeds if at the end of the input, consuming no input
   */
  case object End extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      if (index == input.length) Success((), index)
      else fail(input, index)
    }
  }

  /**
   * Parses a literal `String`
   */
  case class Literal(s: String) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      if (input.startsWith(s, index)) Result.Success(s, index + s.length)
      else fail(input, index)
    }
    override def toString = literalize(s).toString
  }

  /**
   * Parses a single character
   */
  case class CharLiteral(c: Char) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      if (index >= input.length) fail(input, index)
      else if (input(index) == c) Result.Success(c.toString, index + 1)
      else fail(input, index)
    }
    override def toString = literalize(c.toString).toString
  }


  var logNesting = 0

  /**
   * Wraps a parser and prints out the indices where it starts
   * and ends, together with its result
   */
  case class Logged[+T](p: Parser[T], msg: String) extends Parser[T]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      val indent = "  " * logNesting
      println(indent + "+" + msg + ":" + index)
      logNesting += 1
      val res = p.parseRecursive(input, index, logDepth + 1)
      logNesting -= 1
      println(indent + "-" + msg + ":" + index + ":" + res)
      res
    }
  }


  /**
   * A top-level, named parser.
   */
  case class Rule[+T](name: String, p: () => Parser[T]) extends Parser[T]{
    lazy val pCached = p()
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      pCached.parseRecursive(input, index, logDepth) match{
        case f: Failure => failMore(f, index)
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
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      p.parseRecursive(input, index, logDepth) match{
        case s: Success[_] => Result.Success((), index)
        case f: Failure => failMore(f, index)
      }
    }
    override def toString = s"&($p)"
  }
  /**
   * Wraps another parser, succeeding it it fails and failing
   * if it succeeds. Neither case consumes any input
   */
  case class Not(p: Parser[_]) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      val res0 = p.parseRecursive(input, index, logDepth)
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

    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      p.parseRecursive(input, index, logDepth) match{
        case Success(t, index, cut) => Success(ev(Some(t)), index, cut)
        case f: Failure if f.cut => failMore(f, index)
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
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      p1.parseRecursive(input, index, logDepth) match{
        case f: Failure => failMore(f, index, cut = f.cut)
        case s1: Success[_] =>
          p2.parseRecursive(input, s1.index, logDepth) match{
          case f: Failure => failMore(f, index, cut = cut || f.cut || s1.cut)
          case s2: Success[_] => Success(ev(s1.t, s2.t), s2.index, s2.cut || s1.cut | cut)
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
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      val res = mutable.Buffer.empty[T]
      var finalIndex = index
      var lastFailure: Failure = null
      @tailrec def rec(index: Int, del: Parser[_]): Unit = {
        del.parseRecursive(input, index, logDepth) match{
          case f: Failure if f.cut => lastFailure = failMore(f, index)
          case f: Failure => lastFailure = f
          case Success(t, i, cut1) =>
            p.parseRecursive(input, i, logDepth) match{
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
        failMore(lastFailure, index)
      }
      else if (res.length >= min) Success(ev(res.iterator), finalIndex)
      else fail(input, index)
    }
    override def toString = {
      p + ".rep" + (if (min == 0) "" else min) + (if (delimiter == Pass) "" else s"($delimiter)")
    }
  }

  /**
   * Parses using one parser or the other, if the first one fails. Returns 
   * the first one that succeeds and fails if both fail
   */
  case class Either[V](p1: Parser[V], p2: Parser[V]) extends Parser[V]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      p1.parseRecursive(input, index, logDepth) match{
        case s: Success[_] => s
        case f: Failure if f.cut => failMore(f, index)
        case _ => p2.parseRecursive(input, index, logDepth) match{
          case s: Success[_] => s
          case f: Failure if f.cut => failMore(f, index)
          case f: Failure => fail(input, index)
        }
      }
    }
    override def toString = {
      def rec(p: Parser[_]): String = p match {
        case p: Either[_] => rec(p.p1) + " | " + rec(p.p2)
        case p => p.toString
      }
      "(" + rec(this) + ")"
    }
  }

  /**
   * Parses a single character if it passes the predicate
   */
  case class CharPred(predicate: Char => Boolean) extends Parser[Unit]{
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
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
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
      if (index >= input.length) fail(input, index)
      else if (uberSet(input(index))) Result.Success(input(index), index + 1)
      else fail(input, index)
    }
    override def toString = {
      s"CharSets(${literalize(strings.flatten.mkString)})"
    }
  }

  /**
   * Very efficiently attempts to parse a set of strings, by
   * first converting it into a Trie and then walking it once.
   * If multiple strings match the input, longest match wins.
   */
  case class CharTrie(strings: String*) extends Parser[Unit]{
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
    def parseRecursive(input: String, index: Int, logDepth: Int) = {
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
      s"CharTrie(${strings.map(literalize(_)).mkString(", ")})"
    }
  }
}

