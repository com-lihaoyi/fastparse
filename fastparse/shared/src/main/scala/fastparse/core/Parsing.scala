package fastparse
package core
import acyclic.file
import fastparse.Utils._

/**
 * A single frame of the parser call stack
 *
 * @param index The index at which point this parse frame started
 * @param parser The parser which was attempted at this index
 */
case class Frame(index: Int, parser: Parser[_])

/**
 * Result of a parse, whether successful or failed
 */
sealed trait Result[+T]{
  /**
   * Where the parser ended up, whether the result was a success or failure
   */
  def index: Int
  def get: Result.Success[T] = this match{
    case s: Result.Success[T] => s
    case f: Result.Failure => throw new Exception(f.trace)
  }

}

object Result{
  /**
   * @param value The result of this parse
   * @param index The index where the parse completed; may be less than
   *              the length of input
   */
  case class Success[+T](value: T, index: Int) extends Result[T]
  /**
   * @param input The input string for the failed parse. Useful so the [[Failure]]
   *              object can pretty-print snippet
   * @param fullStack The entire stack trace where the parse failed, containing every
   *                  parser in the stack and the index where the parser was used, excluding
   *                  the final parser and index where the parse failed. Only set if
   *                  `parse` is called with `trace = true`, otherwise empty
   * @param index The index in the parse where this parse failed
   * @param lastParser The deepest parser in the parse which failed
   * @param traceParsers A list of parsers that could have succeeded at the location
   *                     that this
   */
  case class Failure(input: String,
                     fullStack: List[Frame],
                     index: Int,
                     lastParser: Parser[_],
                     traceParsers: List[Parser[_]]) extends Result[Nothing]{



    /**
     * A slimmed down version of [[fullStack]], this only includes named
     * [[parsers.Combinators.Rule]] objects as well as the final Parser (whether named or not)
     * and index where the parse failed for easier reading.
     */
    lazy val stack = {
      fullStack.collect {
        case f@Frame(i, p) if p.shortTraced => f
      } :+ Frame(
        index,
        fastparse.parsers.Combinators.Either(traceParsers.distinct:_*)
      )
    }

    /**
     * A longer version of [[trace]], which shows more context
     * for every stack frame
     */
    lazy val  verboseTrace = {
      val body =
        for (Frame(index, p) <- stack)
          yield s"$index\t...${literalize(input.slice(index, index + 5))}\t$p"
      body.mkString("\n")
    }

    /**
     * A one-line snippet that tells you what the state of the
     * parser was when it failed
     */
    lazy val  trace = {
      val body =
        for (Frame(index, p) <- stack)
          yield s"${Precedence.opWrap(p, Precedence.`:`)}:$index"

      body.mkString(" / ") + " ..." + literalize(input.slice(index, index + 10))
    }

    override def toString = s"Failure($trace)"
  }
  object Failure {
    def unapply[T](x: Result[T]) = x match{
      case s: Failure => Some((s.lastParser, s.index))
      case _ => None
    }

  }
}
/**
 * An internal mirror of the [[Result]] classes, except it contains far
 * more data and is mutable to maximize performance
 */
trait Mutable[+T]{
  /**
   * Snapshots this mutable result and converts it into
   * an immutable [[Result]] object
   */
  def toResult: Result[T]

  /**
   * A set of parsers which have failed to parse at
   * the configured `traceIndex`. This is used to query,
   * at any particular index, what parsers could have
   * succeeded.
   */
  def traceParsers: List[Parser[_]]

  /**
   * Whether or not the parser encountered a Cut before reaching
   * this success, which prevents backtracking.
   */
  def cut: Boolean
}
object Mutable{

  case class Success[T](var value: T,
                        var index: Int,
                        var traceParsers: List[Parser[_]],
                        var cut: Boolean = false) extends Mutable[T]{
    override def toString = s"Success($value, $index)"
    def toResult = Result.Success(value, index)
  }
  def unapply[T](x: Result[T]) = x match{
    case s: Success[T] => Some((s.value, s.index))
    case _ => None
  }

  /**
   * A mutable version of [[Result.Failure]] with extra data.
   * @param originalParser the original parser that was attempted and failed.
   *                       Used to repeat the parse with tracing when
   *                       generating error messages
   * @param originalIndex The original index that [[originalParser]] was
   *                      attemped at
   * @param traceIndex The index at which parser traces are required; -1 if empty
   */
  case class Failure(var input: String,
                     var fullStack: List[Frame],
                     var index: Int,
                     var lastParser: Parser[_],
                     originalParser: Parser[_],
                     originalIndex: Int,
                     traceIndex: Int,
                     var traceParsers: List[Parser[_]],
                     var cut: Boolean) extends Mutable[Nothing]{
    def toResult = {
      val (fullStack1, traceParsers1) = {
        if (traceParsers != Nil) (traceParsers, fullStack)
        else {
          val traced = originalParser.parseRec(
            ParseCtx(input, 0, index, originalParser, originalIndex, (_, _, _) => ()),
            originalIndex
          ).asInstanceOf[Mutable.Failure]

          (traced.traceParsers, traced.fullStack)
        }
      }

      Result.Failure(input, traceParsers1, index, lastParser, fullStack1)
    }
  }
}

import fastparse.core.Result._

/**
 * Things which get passed through the entire parse run, but almost never
 * get changed in the process.
 *
 * @param input The string that is currently being parsed
 * @param logDepth
 */
case class ParseCtx(input: String,
                    logDepth: Int,
                    traceIndex: Int,
                    originalParser: Parser[_],
                    originalIndex: Int,
                    instrument: (Parser[_], Int, () => Result[_]) => Unit){
  val failure = Mutable.Failure(input, Nil, 0, null, originalParser, originalIndex, traceIndex, Nil, false)
  val success = Mutable.Success(null, 0, Nil, false)
}


/**
 * A single, self-contained, immutable parser. The primary method is
 * `parse`, which returns a [[T]] on success and a stack trace on failure.
 *
 * Some small optimizations are performed in-line: collapsing [[parsers.Combinators.Either]]
 * cells into large ones and collapsing [[parsers.Combinators.Sequence]] cells into
 * [[parsers.Combinators.Sequence.Flat]]s. These optimizations together appear to make
 * things faster but any 10%, whether or not you activate tracing.
 */
trait Parser[+T] extends ParserApi[T] with Precedence{
  /**
   * Parses the given `input` starting from the given `index`
   *
   * @param input The string we want to parse
   *
   * @param index The index in the string to start from. By default parsing
   *              starts from the beginning of a string, but you can start
   *              from halfway through the string if you want.
   *
   * @param traceFailure Whether or not you want a full stack of any error
   *                     messages that appear. Without it, you only get the
   *                     single deepest parser in the call-stack when it
   *                     failed, and its index. With `trace`, you get every
   *                     parser all the way to the top, as well as every
   *                     possible parser that could have succeeded at the
   *                     location of the error. The downside is that it takes
   *                     an extra parse to generate the error and thus slows
   *                     down failed-parses by a factor of 2-3.
   *
   * @param instrument Allows you to pass in a callback that will get called
   *                   by every named rule, its index, as it itself given a
   *                   callback that can be used to recurse into the parse and
   *                   return the result. Very useful for extracting auxiliary
   *                   information from the parse, e.g. counting rule
   *                   invocations to locate bottlenecks or unwanted
   *                   backtracking in the parser.
   */
  def parse(input: String,
            index: Int = 0,
            traceFailure: Boolean = false,
            instrument: (Parser[_], Int, () => Result[_]) => Unit = null)
            : Result[T] = {
    parseRec(ParseCtx(input, 0, -1, this, index, instrument), index).toResult

  }

  /**
   * Parses the given `input` starting from the given `index` and `logDepth`
   */
  def parseRec(cfg: ParseCtx, index: Int): Mutable[T]

  /**
   * Whether or not this parser should show up when [[Failure.trace]] is called
   */
  def shortTraced: Boolean = false

  /**
   * Whether or not to surround this parser with parentheses when printing.
   * By default a top-level parser is always left without parentheses, but
   * if a sub-parser is embedded inside with lower precedence, it will be
   * surrounded. Set to `Integer.MaxValue` to never be parenthesized
   */
  def opPred: Int = Precedence.Max
}

trait ParserApi[+T]{ this: Parser[T] =>
  def fail(f: Mutable.Failure,
           index: Int,
           traceParsers: List[Parser[_]] = List(this),
           cut: Boolean = false) = {
    f.index = index
    f.cut = cut
    f.fullStack = Nil
    if (f.traceIndex != -1 && f.traceIndex >= index) {
      if (f.traceIndex == index) {
        f.traceParsers = traceParsers
      } else {
        f.traceParsers = Nil
      }
    }
    f.lastParser = this
    f
  }

  def failMore(f: Mutable.Failure,
               index: Int,
               traceParsers: List[Parser[_]],
               cut: Boolean = false) = {

    if (f.traceIndex != -1) {
      if (index >= f.traceIndex) {
        f.traceParsers = traceParsers
      }
      f.fullStack = new Frame(index, this) :: f.fullStack
    }
    f.cut = f.cut | cut
    f
  }

  def success[T](s: Mutable.Success[_],
                 value: T,
                 index: Int,
                 traceParsers: List[Parser[_]],
                 cut: Boolean) = {
    val s1 = s.asInstanceOf[Mutable.Success[T]]

    s1.value = value
    s1.index = index
    s1.cut = cut
    s.traceParsers = traceParsers
    s1
  }
}