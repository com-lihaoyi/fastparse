package fastparse

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
    def get = throw new Exception("Parse Error, " + trace)
    def fold[V](onFailure: (String, Int, Extra) => V, onSuccess: (Nothing, Int) => V) = onFailure(parseFailureString, index, extra)
    override def toString() = s"Parsed.Failure($trace)"
    def trace = {
      parseFailureString match{
        case "" =>
          "Position " + extra.input.prettyIndex(index) +
            ", found " + Failure.formatTrailing(extra.input, index)
        case s =>
          "Expected " + s + ":" + extra.input.prettyIndex(index) +
            ", found " + Failure.formatTrailing(extra.input, index)
      }

    }
    def traced = extra.traced
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
    def traced = {
      input.checkTraceable()
      TracedFailure.fromParsingRun(
        parseInputRaw[Any](
          input,
          originalParser,
          startIndex = startIndex,
          traceIndex = index,
          enableLogging = true,
          verboseFailures = true
        )
      )
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
    def trace = {
      "Expected " + Failure.formatStack(input, stack ++ Seq(aggregateMsg -> index)) +
      ", found " + Failure.formatTrailing(input, index)
    }
  }
}

