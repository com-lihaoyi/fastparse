package fastparse
package core
import acyclic.file

import fastparse.utils._

import scala.collection.mutable
import scala.reflect.ClassTag
/**
 * A single frame of the parser call stack
 *
 * @param index The index at which point this parse frame started
 * @param parser The parser which was attempted at this index
 */
case class Frame(index: Int, parser: Parser[_, _, _])

/**
 * Result of a parse, whether successful or failed.
 */
sealed trait Parsed[+T, Elem, Repr]{
  /**
   * Where the parser ended up, whether the result was a success or failure
   */
  def index: Int

  /**
   * Converts this instance of [[Parsed]] into a [[Parsed.Success]] or
   * throws an exception if it was a failure.
   */
  def get: Parsed.Success[T, Elem, Repr] = this match{
    case s: Parsed.Success[T, Elem, Repr] => s
    case f: Parsed.Failure[Elem, Repr] => throw new ParseError[Elem, Repr](f)
  }

  /**
    * Returns the result of $onSuccess if the parsing is successful, otherwise it returns the result of $onFailure
    */
  def fold[X](onFailure: (Parser[_, Elem, Repr], Int, Parsed.Failure.Extra[Elem, Repr]) => X, onSuccess: (T, Int) => X): X = this match {
    case Parsed.Success(t, i) => onSuccess(t, i)
    case f: Parsed.Failure[Elem, Repr] => onFailure(f.lastParser, f.index, f.extra)
  }
}

case class ParseError[Elem, Repr](failure: Parsed.Failure[Elem, Repr]) extends Exception(
  failure.extra.input.repr.errorMessage(failure.extra.input, failure.extra.traced.expected, failure.index)
)

object ParseError{
  def msg[Elem, Repr](input: ParserInput[Elem, Repr], expected: String, idx: Int) = {
    "SyntaxError: " + input.repr.errorMessage(input, expected, idx)
  }
}

object Parsed {

  /**
   * @param value The result of this parse
   * @param index The index where the parse completed; may be less than
   *              the length of input
   */
  case class Success[+T, Elem, Repr](value: T, index: Int) extends Parsed[T, Elem, Repr]

  /**
   * Simple information about a parse failure. Also contains the original parse
   * information necessary to construct the traced failure. That contains more
   * information but is more costly to compute and is thus computed lazily on
   * demand.
   *
   * @param index The index in the parse where this parse failed
   * @param lastParser The deepest parser in the parse which failed
   * @param extra Extra supplementary information (including trace information).
   *              For details see [[Parsed.Failure.Extra]]
   */
  case class Failure[Elem, Repr](lastParser: Parser[_, Elem, Repr],
                                 index: Int,
                                 extra: Failure.Extra[Elem, Repr]) extends Parsed[Nothing, Elem, Repr]{

    def msg = Failure.formatStackTrace(
      Nil, extra.input, index, Failure.formatParser(lastParser, extra.input, index)
    )

    override def toString = s"Failure($msg)"
  }

  object Failure {
    /**
      * Additional tracing information on a parse failure.
      * Use `traced` to obtain the [[TracedFailure]],see [[TracedFailure]] for further details.
      */
    sealed trait Extra[Elem, Repr] {
      /** Input string. */
      def input: ParserInput[Elem, Repr]
      /** Get the underlying [[TracedFailure]] to allow for analysis of the full parse stack. */
      def traced: TracedFailure[Elem, Repr]
    }

    object Extra{
      class Impl[Elem, Repr](val input: ParserInput[Elem, Repr],
                             startParser: Parser[_, Elem, Repr], startIndex: Int,
                             lastParser: Parser[_, Elem, Repr], index: Int) extends Extra[Elem, Repr] {

        lazy val traced = {
          input.checkTraceable()
          TracedFailure(input, index, lastParser, (startIndex, startParser))
        }
        override def toString = {
          val inputHead = {
            val ellipses = if (input.innerLength < 20 && input.innerLength == input.length) "" else "..."
            ellipses + input.slice(input.length - 20, input.length)
          }
          s"Extra($inputHead, [traced - not evaluated])"
        }
      }
    }


    def formatParser[Elem, Repr](p: Precedence, input: ParserInput[Elem, Repr], index: Int) = {
      s"${Precedence.opWrap(p, Precedence.`:`)}:${input.repr.prettyIndex(input, index)}"
    }

    def formatStackTrace[Elem, Repr](stack: Seq[Frame],
                                     input: ParserInput[Elem, Repr],
                                     index: Int,
                                     last: String) = {
      val body =
        for (Frame(index, p) <- stack)
        yield formatParser(p, input, index)
      (body :+ last).mkString(" / ") + " ..." + input.repr.literalize(input.slice(index, index + 10))
    }
    def filterFullStack(fullStack: Seq[Frame]) = {
      fullStack.collect { case f @ Frame(i, p) if p.shortTraced => f }
    }
  }

  // TracedFailure
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
  case class TracedFailure[Elem, Repr](input: ParserInput[Elem, Repr],
                                       index: Int,
                                       fullStack: Vector[Frame],
                                       traceParsers: Set[Parser[_, Elem, Repr]]) {

    private[this] lazy val expected0 = new Precedence {
      def opPred = if (traceParsers.size == 1) traceParsers.head.opPred else Precedence.|
      override def toString = traceParsers.map(opWrap).mkString(" | ")
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
      Failure.formatStackTrace(stack, input, index, Failure.formatParser(expected0, input, index))
    }
  }
  object TracedFailure{
    def apply[Elem, Repr](input: ParserInput[Elem, Repr], index: Int,
                          lastParser: Parser[_, Elem, Repr], traceData: (Int, Parser[_, Elem, Repr])) = {
      val (originalIndex, originalParser) = traceData

      val mutFailure = originalParser.parseRec(
        new ParseCtx(input, 0, index, originalParser, originalIndex, (_, _, _) => (), false, false, false),
        originalIndex
      ).asInstanceOf[Mutable.Failure[Elem, Repr]]


      new TracedFailure[Elem, Repr](
        input,
        index,
        mutFailure.fullStack.toVector.reverse,
        mutFailure.traceParsers + lastParser
      )
    }
  }
}
/**
 * An internal mirror of the [[Parsed]] classes, except it contains far
 * more data and is mutable to maximize performance
 */
trait Mutable[+T, Elem, Repr]{
  /**
   * Snapshots this mutable result and converts it into
   * an immutable [[Parsed]] object
   */
  def toResult: Parsed[T, Elem, Repr]

  /**
   * A set of parsers which have failed to parse at
   * the configured `traceIndex`. This is used to query,
   * at any particular index, what parsers could have
   * succeeded.
   */
  def traceParsers: Set[Parser[_, Elem, Repr]]

  /**
   * Whether or not the parser encountered a Cut before reaching
   * this success, which prevents backtracking.
   */
  def cut: Boolean
  def cut_=(c: Boolean): Unit
}
object Mutable{

  /**
   * A mutable version of [[Parsed.Success]] with extra data.
    *
    * @param traceParsers If a `traceIndex` is provided, this will contain any
   *                     parsers within this [[Success]] that failed at exactly
   *                     that index, which will be used for error reporting.
   *                     If you are writing your own custom [[Parser]] and it
   *                     contains sub-parsers, you should generally aggregate
   *                     any the [[traceParsers]] of any of their results.
   * @param cut Whether or not this parser crossed a cut and can not longer
   *            backtrack
   */
  case class Success[T, Elem, Repr](var value: T,
                                    var index: Int,
                                    var traceParsers: Set[Parser[_, Elem, Repr]],
                                    var cut: Boolean = false) extends Mutable[T, Elem, Repr]{

    override def toString = s"Success($value, $index)"
    def toResult = Parsed.Success(value, index)
  }


  /**
   * A mutable version of [[Parsed.Failure]] with extra data.
   *
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
  case class Failure[Elem, Repr](var input: ParserInput[Elem, Repr],
                                 fullStack: mutable.Buffer[Frame],
                                 var index: Int,
                                 var lastParser: Parser[_, Elem, Repr],
                                 originalParser: Parser[_, Elem, Repr],
                                 originalIndex: Int,
                                 traceIndex: Int,
                                 var traceParsers: Set[Parser[_, Elem, Repr]],
                                 var cut: Boolean) extends Mutable[Nothing, Elem, Repr]{
    def toResult = {
      val extra = new Parsed.Failure.Extra.Impl(input, originalParser, originalIndex, lastParser, index)
      Parsed.Failure(lastParser, index, extra)
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
case class ParseCtx[Elem, Repr](input: ParserInput[Elem, Repr],
                                var logDepth: Int,
                                traceIndex: Int,
                                originalParser: Parser[_, Elem, Repr],
                                originalIndex: Int,
                                instrument: (Parser[_, Elem, Repr], Int, () => Parsed[_, Elem, Repr]) => Unit,
                                var isFork: Boolean,
                                var isCapturing: Boolean,
                                var isNoCut: Boolean) {
  require(logDepth >= -1, "logDepth can only be -1 (for no logs) or >= 0")
  require(traceIndex >= -1, "traceIndex can only be -1 (for no tracing) or an index 0")
  val failure = Mutable.Failure[Elem, Repr](
    input, mutable.Buffer(), 0, null, originalParser,
    originalIndex, traceIndex, Set.empty, false
  )
  val success = Mutable.Success[Any, Elem, Repr](null, 0, Set.empty, false)

  def checkForDrop(outerCut: Boolean) = !isCapturing && ((outerCut && !isNoCut) || !isFork)
}

// Parser
/**
 * A single, self-contained, immutable parser. The primary method is
 * `parse`, which returns a [[T]] on success and a stack trace on failure.
 *
 * Some small optimizations are performed in-line: collapsing
 * [[parsers.Combinators.Either]] cells into large ones and collapsing
 * [[parsers.Combinators.Sequence]] cells into
 * [[parsers.Combinators.Sequence.Flat]]s. These optimizations together appear
 * to make things faster but any 10%, whether or not you activate tracing.
 */
abstract class Parser[+T, Elem, Repr]()(implicit val reprOps: ReprOps[Elem, Repr])
  extends ParserResults[T, Elem, Repr] with Precedence{

  /**
    * Can be passed into a `.parse` call to let you inject logic around
    * the parsing of top-level parsers, e.g. for logging and debugging.
    */
  type InstrumentCallback = (
    (Parser[_, Elem, Repr], Int, () => Parsed[_, Elem, Repr]) => Unit
  )

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
  def parse(input: Repr,
            index: Int = 0,
            instrument: InstrumentCallback = null)
      : Parsed[T, Elem, Repr] = {
    parseInput(IndexedParserInput(input), index, instrument)
  }

  def parseIterator(input: Iterator[Repr],
                    index: Int = 0,
                    instrument: InstrumentCallback = null)
                   (implicit ct: ClassTag[Elem])
      : Parsed[T, Elem, Repr] = {
    parseInput(IteratorParserInput(input), index, instrument)
  }

  def parseInput(input: ParserInput[Elem, Repr],
                 index: Int = 0,
                 instrument: InstrumentCallback = null)

      : Parsed[T, Elem, Repr] = {
    parseRec(
      new ParseCtx(input, 0, -1, this, index, instrument, false, false, false),
      index
    ).toResult
  }

  /**
   * Extractor for pattern matching
   *
   *  For example:
   *
   *  {{{
   *  val p1 = CharIn('a'to'z').! ~ CharIn('0'to'9').rep(1).!.map(_.toInt)
   *  List("a42x") match {
   *    case p1(x: String, y: Int) :: _ => // x is "a", y is 42
   *  }
   * }}}
   */
  def unapply(input: Repr): Option[T] = {
    parse(input) match {
      case Parsed.Success(r, _) => Some(r)
      case _ => None
    }
  }

  /**
   * Parses the given `input` starting from the given `index` and `logDepth`
   */
  def parseRec(cfg: ParseCtx[Elem, Repr], index: Int): Mutable[T, Elem, Repr]

  /**
   * Whether or not this parser should show up when
   * [[Parsed.TracedFailure.trace]] is called. If not set, the parser will
    * only show up in [[Parsed.TracedFailure.fullStack]]
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
trait ParserResults[+T, Elem, Repr]{ this: Parser[T, Elem, Repr] =>
  def mergeTrace(traceIndex: Int, lhs: Set[Parser[_, Elem, Repr]],
                 rhs: Set[Parser[_, Elem, Repr]]): Set[Parser[_, Elem, Repr]] = {
    if (traceIndex != -1) lhs | rhs
    else Set.empty
  }
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
  def fail(f: Mutable.Failure[Elem, Repr],
           index: Int,
           traceParsers: Set[Parser[_, Elem, Repr]] = null,
           cut: Boolean = false) = {
    f.index = index
    f.cut = cut
    f.fullStack.clear()
    if (f.traceIndex != -1 && f.traceIndex >= index) {
      if (f.traceIndex == index) {
        f.traceParsers =
          if (traceParsers == null) Set(this)
          else traceParsers
      } else {
        f.traceParsers = Set.empty
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
  def failMore(f: Mutable.Failure[Elem, Repr],
               index: Int,
               logDepth: Int,
               traceParsers: Set[Parser[_, Elem, Repr]] = null,
               cut: Boolean = false): Mutable.Failure[Elem, Repr] = {

    if (f.traceIndex != -1) {
      if (index >= f.traceIndex && traceParsers != null) {
        f.traceParsers = traceParsers
      }
    }
    // Record the failure stack if we're tracing or if
    // we're logging, otherwise ignore it for performance.
    if (f.traceIndex != -1 || logDepth > 0){
      f.fullStack += new Frame(index, this)
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
  def success[T](s: Mutable.Success[_, Elem, Repr],
                 value: T,
                 index: Int,
                 traceParsers: Set[Parser[_, Elem, Repr]],
                 cut: Boolean) = {
    val s1 = s.asInstanceOf[Mutable.Success[T, Elem, Repr]]

    s1.value = value
    s1.index = index
    s1.cut = cut
    s.traceParsers = traceParsers
    s1
  }
}
