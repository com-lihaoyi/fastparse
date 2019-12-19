package fastparse

import fastparse.internal.{Msgs, Util}

/**
  * The outcome of a [[ParsingRun]] run, either a success (with value and index) or
  * failure (with associated debugging metadata to help figure out what went
  * wrong).
  *
  * Doesn't contain any information not already present in [[ParsingRun]], but
  * packages it up nicely in an immutable case class that's easy for external
  * code to make use of.
  */
sealed abstract class Parsed[+T](val isSuccess: Boolean){
  def fold[V](onFailure: (String, Int, Parsed.Extra) => V, onSuccess: (T, Int) => V): V
  def get: Parsed.Success[T]
}

object Parsed{
  def fromParsingRun[T](p: ParsingRun[T]): Parsed[T] = {
    if (p.isSuccess) Parsed.Success(p.successValue.asInstanceOf[T], p.index)
    else Parsed.Failure(
      Option(p.lastFailureMsg).fold("")(_.render),
      p.index,
      new Parsed.Extra(p.input, p.startIndex, p.index, p.originalParser, p.failureStack)
    )
  }

  /**
    * The outcome of a successful parse
    *
    * @param value The value returned by the parse
    * @param index The index at which the parse completed at
    */
  final case class Success[+T](value: T, index: Int) extends Parsed[T](true){
    def get = this
    def fold[V](onFailure: (String, Int, Extra) => V, onSuccess: (T, Int) => V) = onSuccess(value, index)
    override def toString() = s"Parsed.Success($value, $index)"
  }

  /**
    * The outcome of a failed parse
    *
    * @param label A hint as to why the parse failed. Defaults to "",
    *                     unless you set `verboseFailures = true` or call
    *                     `.trace()` on an existing failure
    * @param index The index at which the parse failed
    * @param extra Metadata about the parse; useful for re-running the parse
    *              to trace out a more detailed error report
    */
  final class Failure(val label: String,
                      val index: Int,
                      val extra: Extra) extends Parsed[Nothing](false){
    def get = throw new Exception("Parse Error, " + msg)
    def fold[V](onFailure: (String, Int, Extra) => V, onSuccess: (Nothing, Int) => V) = onFailure(label, index, extra)
    override def toString() = s"Parsed.Failure($msg)"

    /**
      * Displays the failure message excluding the parse stack
      */
    def msg = {
      label match{
        case "" =>
          "Position " + extra.input.prettyIndex(index) +
          ", found " + Failure.formatTrailing(extra.input, index)
        case s => Failure.formatMsg(extra.input, List(s -> index), index)
      }
    }

    /**
      * Displays the failure message including the parse stack, if possible
      */
    def longMsg = {
      if (extra.stack.nonEmpty) {
        Failure.formatMsg(extra.input, extra.stack ++ List(label -> index), index)
      } else throw new Exception(
        "`.longMsg` requires the parser to be run with `verboseFailures = true`, " +
        "or to be called via `.trace().longMsg` or `.trace().longAggregateMsg`"
      )
    }

    /**
      * Re-runs the failed parse with `verboseFailures` turned on and failure
      * aggregation enabled. This allows Fastparse to provide much more
      * detailed error messages, at a cost of taking ~2x as long than the
      * original parse.
      *
      * By default, logging is disabled during the tracing run; this is because
      * you typically already saw the logging output during the primary parse,
      * and the tracing run's log output should be identical. You can pass in
      * `enableLogging = true` to log the tracing run as well.
      */
    def trace(enableLogging: Boolean = false) = extra.trace(enableLogging)
  }

  object Failure{
    def apply(label: String, index: Int, extra: Extra) = new Failure(label, index, extra)
    def unapply(x: Failure): Option[(String, Int, Extra)] = x match{
      case f: Failure => Some((f.label, f.index, f.extra))
      case _ => None
    }
    def formatMsg(input: ParserInput, stack: List[(String, Int)], index: Int) = {
      "Expected " + Failure.formatStack(input, stack) +
      ", found " + Failure.formatTrailing(input, index)
    }
    def formatStack(input: ParserInput, stack: List[(String, Int)]) = {
      stack.map{case (s, i) => s"$s:${input.prettyIndex(i)}"}.mkString(" / ")
    }
    def formatTrailing(input: ParserInput, index: Int) = {
      Util.literalize(input.slice(index, index + 10))
    }
  }

  class Extra(val input: ParserInput,
              val startIndex: Int,
              val index: Int,
              val originalParser: ParsingRun[_] => ParsingRun[_],
              val stack: List[(String, Int)]) {
    @deprecated("Use .trace instead")
    def traced = trace()

    /**
      * Re-runs the failed parse with aggregation turned on. This is the
      * slowest of Fastparse's error reporting mode, taking ~2x as long
      * as the original parse, but provides the greatest detail in the error
      * message
      *
      * By default, logging is disabled during the tracing run; this is because
      * you typically already saw the logging output during the primary parse,
      * and the tracing run's log output should be identical. You can pass in
      * `enableLogging = true` to log the tracing run as well.
      */
    def trace(enableLogging: Boolean = false) = {
      input.checkTraceable()
      TracedFailure.fromParsingRun(
        parseInputRaw[Any](
          input,
          originalParser,
          startIndex = startIndex,
          traceIndex = index,
          enableLogging = enableLogging,
          verboseFailures = true
        )
      )
    }
  }

  object TracedFailure{

    def fromParsingRun[T](p: ParsingRun[T]) = {
      assert(!p.isSuccess)
      TracedFailure(
        p.failureTerminalAggregate,
        p.lastFailureMsg ::: p.failureGroupAggregate,
        Parsed.fromParsingRun(p).asInstanceOf[Failure]
      )
    }
  }

  /**
    * A decorated [[Failure]] with extra metadata; provides a much more
    * detailed, through verbose, of the possible inputs that may have been
    * expected at the index at which the parse failed.
    *
    * @param terminals A list of all the lowest level parsers which could have
    *                  succeeded at the failure index. These are things like
    *                  literal string parsers, [[CharIn]], [[CharPred]], etc.
    * @param groups A list of all the highest level parsers which could have
    *               succeeded at the given failure index. These give you a
    *               good
    * @param failure The raw failure object
    */
  case class TracedFailure(terminals: Msgs,
                           groups: Msgs,
                           failure: Failure){
    def label = failure.label
    def index = failure.index
    def input = failure.extra.input
    def stack = failure.extra.stack
    def terminalAggregateString = terminals.render

    def groupAggregateString = groups.render

    @deprecated("Use .msg instead")
    def trace = aggregateMsg
    /**
      * Displays the short failure message excluding the parse stack. This shows
      * the last parser which failed causing the parse to fail. Note that this
      * does not include other parsers which may have failed earlier; see [[terminalsMsg]]
      * and [[aggregateMsg]] for more detailed errors
      */
    def msg = failure.msg
    /**
      * Displays the terminals failure message, excluding the parse stack. This
      * includes a list of all lowest-level parsers which could have succeeded
      * at the failure index: literal strings, [[CharIn]], [[CharPred]]s, etc.
      * This gives you a detailed listing of how the parse could be corrected,
      * though it can be verbose.
      */
    def terminalsMsg = Failure.formatMsg(input, List(terminalAggregateString -> index), index)

    /**
      * Displays the aggregate failure message, excluding the parse stack. This
      * includes a list of all highest-level parsers which could have succeeded
      * at the failure index. This gives you a good high-level overview of what
      * the parser expected, at the cost
      */
    def aggregateMsg = Failure.formatMsg(input, List(groupAggregateString -> index), index)

    /**
      * A version of [[msg]] that includes the parse stack
      */
    def longMsg = failure.longMsg

    /**
      * A version of [[terminalsMsg]] that includes the parse stack.
      */
    def longTerminalsMsg = Failure.formatMsg(input, stack ++ Seq(terminalAggregateString -> index), index)

    /**
      * A version of [[aggregateMsg]] that includes the parse stack
      */
    def longAggregateMsg = Failure.formatMsg(input, stack ++ Seq(groupAggregateString -> index), index)
  }
}

