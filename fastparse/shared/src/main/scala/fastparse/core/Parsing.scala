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

  /**
   * Converts this [[Result]] into a [[Result.Success]] or throws an exception
   * if it was a failure.
   */
  def get: Result.Success[T] = this match{
    case s: Result.Success[T] => s
    case f: Result.Failure => throw new SyntaxError(f)
  }
}

case class SyntaxError(failure: Result.Failure) extends Exception{
  override def toString() = SyntaxError.msg(failure.input, failure.traced.expected, failure.index)
}

object SyntaxError{
  def msg(code: CharSequence, expected: String, idx: Int) = {
    val locationString = {
      val (first, last) = code.toString.splitAt(idx)
      val lastSnippet = last.split('\n').headOption.getOrElse("")
      val firstSnippet = first.reverse.split('\n').lift(0).getOrElse("").reverse
      firstSnippet + lastSnippet + "\n" + (" " * firstSnippet.length) + "^"
    }
    val literal = fastparse.Utils.literalize(code.slice(idx, idx + 20))
    s"SyntaxError: found $literal, expected $expected in\n$locationString"
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
   * Simple information about a parse failure. Also contains the original parse
   * information necessary to construct the traced failure. That contains more
   * information but is more costly to compute and is thus computed lazily on
   * demand.
   * 
   * @param input The input string for the failed parse. Useful so the [[Failure]]
   *              object can pretty-print snippet
   * @param index The index in the parse where this parse failed
   * @param lastParser The deepest parser in the parse which failed
   */
  case class Failure(input: CharSequence,
                     index: Int,
                     lastParser: Parser[_],
                     traceData: (Int, Parser[_])) extends Result[Nothing]{

    lazy val traced = TracedFailure(input, index, lastParser, traceData)

    def msg = Failure.formatStackTrace(
      Nil, input, index, Failure.formatParser(lastParser, index)
    )
    override def toString = s"Failure($msg)"
  }
  object Failure {
    def formatParser(p: Precedence, i: Int) = {
      Precedence.opWrap(p, Precedence.`:`) + ":" + i
    }
    def formatStackTrace(stack: Seq[Frame],
                          input: CharSequence,
                          index: Int,
                          last: String) = {
      val body =
        for (Frame(index, p) <- stack )
        yield formatParser(p, index)
      (body :+ last).mkString(" / ") + " ..." + literalize(input.slice(index, index + 10))
    }
    def filterFullStack(fullStack: Seq[Frame]) = {
      fullStack.collect {
        case f@Frame(i, p) if p.shortTraced => f
      }
    }
    /**
     * Convenience helper to let you pattern match on failures more easily
     */
    def unapply[T](x: Result[T]) = x match{
      case s: Failure => Some((s.lastParser, s.index))
      case _ => None
    }

  }
  // TraceFailure
  /**
   * A failure containing detailed information about a parse failure. This is more
   * expensive to compute than a simple error message and is thus not generated
   * by default.
   *
   * @param fullStack The entire stack trace where the parse failed, containing every
   *                  parser in the stack and the index where the parser was used, excluding
   *                  the final parser and index where the parse failed. Only set if
   *                  `parse` is called with `trace = true`, otherwise empty
   * @param traceParsers A list of parsers that could have succeeded at the location
   *                     that this
   */
  case class TracedFailure(input: CharSequence,
                           index: Int,
                           fullStack: List[Frame],
                           traceParsers: List[Parser[_]]){

    private[this] lazy val expected0 = new Precedence {
      def opPred = if (traceParsers.length == 1) traceParsers(0).opPred else Precedence.|
      override def toString = traceParsers.map(opWrap).distinct.mkString(" | ")
    }

    /**
     * A short string describing the parsers which were expected at the point
     * of failure.
     */
    def expected = expected0.toString

    /**
     * A slimmed down version of [[fullStack]], this only includes named
     * [[parsers.Combinators.Rule]] objects as well as the final Parser (whether named or not)
     * and index where the parse failed for easier reading.
     */
    lazy val stack = Failure.filterFullStack(fullStack)
    
    /**
     * A one-line snippet that tells you what the state of the parser was
     * when it failed. This message is completely derived from other values
     * available on this object, so feel free to use the data yourself if
     * the default error message isn't to your liking.
     */
    lazy val trace = {
      Failure.formatStackTrace(stack, input, index, Failure.formatParser(expected0, index))
    }
  }
  object TracedFailure{
    def apply(input: CharSequence, index: Int, lastParser: Parser[_], traceData: (Int, Parser[_])) = {
      val (originalIndex, originalParser) = traceData

      val mutFailure = originalParser.parseRec(
        new ParseCtx(input, 0, index, originalParser, originalIndex, (_, _, _) => ()),
        originalIndex
      ).asInstanceOf[Mutable.Failure]


      new TracedFailure(
        input,
        index,
        mutFailure.fullStack,
        (mutFailure.traceParsers :+ lastParser).distinct
      )
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

  /**
   * A mutable version of [[Result.Success]] with extra data.
   * @param traceParsers If a `traceIndex` is provided, this will contain any
   *                     parsers within this [[Success]] that failed at exactly
   *                     that index, which will be used for error reporting.
   *                     If you are writing your own custom [[Parser]] and it
   *                     contains sub-parsers, you should generally aggregate
   *                     any the [[traceParsers]] of any of their results.
   * @param cut Whether or not this parser crossed a cut and can not longer
   *            backtrack
   */
  case class Success[T](var value: T,
                        var index: Int,
                        var traceParsers: List[Parser[_]],
                        var cut: Boolean = false) extends Mutable[T]{

    override def toString = s"Success($value, $index)"
    def toResult = Result.Success(value, index)
  }


  /**
   * A mutable version of [[Result.Failure]] with extra data.
   * @param originalParser the original parser that was attempted and failed.
   *                       Used to repeat the parse with tracing when
   *                       generating error messages
   * @param originalIndex The original index that [[originalParser]] was
   *                      attemped at
   * @param traceIndex The index at which parser traces are required; -1 if empty
   * @param traceParsers If a `traceIndex` is provided, this will contain any
   *                     parsers within this [[Failure]] that failed at exactly
   *                     that index, which will be used for error reporting.
   *                     If you are writing your own custom [[Parser]] and it
   *                     contains sub-parsers, you should generally aggregate
   *                     any the [[traceParsers]] of any of their results.
   */
  case class Failure(var input: CharSequence,
                     var fullStack: List[Frame],
                     var index: Int,
                     var lastParser: Parser[_],
                     originalParser: Parser[_],
                     originalIndex: Int,
                     traceIndex: Int,
                     var traceParsers: List[Parser[_]],
                     var cut: Boolean) extends Mutable[Nothing]{
    def toResult = {
      Result.Failure(input, index, lastParser, (originalIndex, originalParser))
    }
  }
}


/**
 * Things which get passed through the entire parse run, but almost never
 * get changed in the process.
 *
 * @param input The string that is currently being parsed
 * @param logDepth How many logging statements we're within, used to properly
 *                 indent log output. Set to `-1` to disable logging
 * @param traceIndex Whether or not to perform full tracing to improve error
 *                   reporting. `-1` disables tracing, and any other number
 *                   enables recording of stack-traces and
 */
class ParseCtx(val input: CharSequence,
               var logDepth: Int,
               val traceIndex: Int,
               val originalParser: Parser[_],
               val originalIndex: Int,
               val instrument: (Parser[_], Int, () => Result[_]) => Unit){
  require(logDepth >= -1, "logDepth can only be -1 (for no logs) or >= 0")
  require(traceIndex >= -1, "traceIndex can only be -1 (for no tracing) or an index 0")
  val failure = Mutable.Failure(input, Nil, 0, null, originalParser, originalIndex, traceIndex, Nil, false)
  val success = Mutable.Success(null, 0, Nil, false)
}

// Parser
/**
 * A single, self-contained, immutable parser. The primary method is
 * `parse`, which returns a [[T]] on success and a stack trace on failure.
 *
 * Some small optimizations are performed in-line: collapsing [[parsers.Combinators.Either]]
 * cells into large ones and collapsing [[parsers.Combinators.Sequence]] cells into
 * [[parsers.Combinators.Sequence.Flat]]s. These optimizations together appear to make
 * things faster but any 10%, whether or not you activate tracing.
 */
trait Parser[+T] extends ParserResults[T] with Precedence{
  /**
   * Parses the given `input` starting from the given `index`
   *
   * @param input The string we want to parse
   *
   * @param index The index in the string to start from. By default parsing
   *              starts from the beginning of a string, but you can start
   *              from halfway through the string if you want.
   *
   * @param instrument Allows you to pass in a callback that will get called
   *                   by every named rule, its index, as it itself given a
   *                   callback that can be used to recurse into the parse and
   *                   return the result. Very useful for extracting auxiliary
   *                   information from the parse, e.g. counting rule
   *                   invocations to locate bottlenecks or unwanted
   *                   backtracking in the parser.
   */
  def parse(input: CharSequence,
            index: Int = 0,
            instrument: (Parser[_], Int, () => Result[_]) => Unit = null)
            : Result[T] = {
    parseRec(new ParseCtx(input, 0, -1, this, index, instrument), index).toResult
  }

  /**
   * Parses the given `input` starting from the given `index` and `logDepth`
   */
  def parseRec(cfg: ParseCtx, index: Int): Mutable[T]

  /**
   * Whether or not this parser should show up when [[TracedFailure.trace]] is
   * called. If not set, the parser will only show up in [[TracedFailure.fullStack]]
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
// End Parser
/**
 * Convenience methods to be used internally inside [[Parser]]s
 */
trait ParserResults[+T]{ this: Parser[T] =>
  /**
   * Prepares a failure object for a new failure
   *
   * @param f The failure object, usually retrieved from the [[ParseCtx]]
   *          to avoid allocation overhead
   * @param index The index at which this failure occurred
   * @param traceParsers Any parsers which failed at the current index. These
   *                     get noted in the error message if `traceFailure` is
   *                     set. By default, this is the current parser.
   * @param cut Whether or not this failure should prevent backtracking
   */
  def fail(f: Mutable.Failure,
           index: Int,
           traceParsers: List[Parser[_]] = null,
           cut: Boolean = false) = {
    f.index = index
    f.cut = cut
    f.fullStack = Nil
    if (f.traceIndex != -1 && f.traceIndex >= index) {
      if (f.traceIndex == index) {
        f.traceParsers =
          if (traceParsers == null) List(this)
          else traceParsers
      } else {
        f.traceParsers = Nil
      }
    }
    f.lastParser = this
    f
  }

  /**
   * Prepares a failure object to continue an existing failure, e.g. if
   * some sub-parser failed and you want to pass the failure up the stack.
   *
   * @param f The failure returned by the subparser
   * @param index The index of the *current* parser
   * @param traceParsers Any parsers which failed at the current index. These
   *                     get noted in the error message if `traceFailure` is
   *                     set. By default, the existing `traceParsers` from the
   *                     original failure are left unchanged
   * @param cut Whether or not this parser failing should prevent backtracking.
   *            ORed with any cuts caused by the existing failure
   */
  def failMore(f: Mutable.Failure,
               index: Int,
               logDepth: Int,
               traceParsers: List[Parser[_]] = null,
               cut: Boolean = false) = {

    if (f.traceIndex != -1) {
      if (index >= f.traceIndex && traceParsers != null) {
        f.traceParsers = traceParsers
      }
    }
    // Record the failure stack if we're tracing or if
    // we're logging, otherwise ignore it for performance.
    if (f.traceIndex != -1 || logDepth > 0){
      f.fullStack = new Frame(index, this) :: f.fullStack
    }
    f.cut = f.cut | cut
    f
  }

  /**
   * Prepares a success object to be returned.
   *
   * @param s The existing success object, usually taken from [[ParseCtx]] to
   *          avoid allocation overhead.
   * @param value The value that this parser succeeded with
   * @param index The index of the parser *after* having successfully parsed
   *              part of the input
   * @param traceParsers Any parsers which failed at the current index in the
   *                     creation of this success. Even though this parser
   *                     succeeded, failures inside sub-parsers must be
   *                     reported to ensure proper error reporting.
   * @param cut Whether the parse crossed a cut and should prevent backtracking
   */
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