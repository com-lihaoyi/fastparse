package fastparse

import fastparse.internal.Util

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
      Option(p.lastFailureMsg).fold("")(_()),
      p.index,
      Parsed.Extra(p.input, p.startIndex, p.index, p.originalParser)
    )
  }
  final case class Success[+T](value: T, index: Int) extends Parsed[T](true){
    def get = this
    def fold[V](onFailure: (String, Int, Extra) => V, onSuccess: (T, Int) => V) = onSuccess(value, index)
    override def toString() = s"Parsed.Success($value, $index)"
  }
  final case class Failure(parseFailureString: String,
                           index: Int,
                           extra: Extra) extends Parsed[Nothing](false){
    def get = throw new Exception("Parse Error, " + msg)
    def fold[V](onFailure: (String, Int, Extra) => V, onSuccess: (Nothing, Int) => V) = onFailure(parseFailureString, index, extra)
    override def toString() = s"Parsed.Failure($msg)"
    def msg = {
      parseFailureString match{
        case "" =>
          "Position " + extra.input.prettyIndex(index) +
            ", found " + Failure.formatTrailing(extra.input, index)
        case s =>
          "Expected " + s + ":" + extra.input.prettyIndex(index) +
            ", found " + Failure.formatTrailing(extra.input, index)
      }
    }

    def traceAggregate() = extra.traceAggregate
    def traceVerbose() = extra.traceVerbose
  }

  object Failure{
    def formatStack(input: ParserInput, stack: List[(String, Int)]) = {
      stack.map{case (s, i) => s"$s:${input.prettyIndex(i)}"}.mkString(" / ")
    }
    def formatTrailing(input: ParserInput, index: Int) = {
      Util.literalize(input.slice(index, index + 10))
    }
  }

  case class Extra(input: ParserInput,
                   startIndex: Int,
                   index: Int,
                   originalParser: ParsingRun[_] => ParsingRun[_]) {
    @deprecated("Use .traceAggregate instead")
    def traced = traceAggregate

    /**
      * Re-runs the failed parse with aggregation turned on. This is the
      * slowest of Fastparse's error reporting mode, taking ~2.5x as long
      * as the original parse, but provides the greatest detail in the error
      * message
      */
    def traceAggregate() = {
      input.checkTraceable()
      TracedFailure.fromParsingRun(
        parseInputRaw[Any](
          input,
          originalParser,
          startIndex = startIndex,
          traceIndex = index,
          enableLogging = false,
          verboseFailures = true
        )
      )
    }

    /**
      * Re-runs the failed parse with `verboseFailures` turned on. This
      * provides a somewhat more detailed error message at a cost of taking
      * ~1.5x as long than the original parse
      */
    def traceVerbose() = {
      input.checkTraceable()
      Parsed.fromParsingRun(
        parseInputRaw[Any](
          input,
          originalParser,
          startIndex = startIndex,
          traceIndex = -1,
          enableLogging = false,
          verboseFailures = true
        )
      ).asInstanceOf[Failure]
    }
  }

  object TracedFailure{

    def fromParsingRun[T](p: ParsingRun[T]) = {
      assert(!p.isSuccess)
      TracedFailure(
        p.failureStack,
        p.index,
        p.input,
        p.failureAggregate.reverse.map(_()).distinct
      )
    }
  }
  case class TracedFailure(stack: List[(String, Int)],
                           index: Int,
                           input: ParserInput,
                           failureAggregate: Seq[String]){
    def aggregateMsg = failureAggregate match{
      case Seq(x) => x
      case items => items.mkString("(", " | ", ")")
    }

    @deprecated("Use .msg instead")
    def trace = msg
    def msg = {
      "Expected " + Failure.formatStack(input, stack ++ Seq(aggregateMsg -> index)) +
      ", found " + Failure.formatTrailing(input, index)
    }
  }
}

