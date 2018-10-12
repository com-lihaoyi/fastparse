package fastparse

import scala.annotation.unchecked.uncheckedVariance


/**
  * Models an in-progress parsing run; contains all the mutable state that may
  * be necessary during the parse, in order to avoid the individual parsers
  * needing to perform their own allocations and instantiations.
  *
  * The value for the fields [[cut]] and [[noDropBuffer]] are lexically scoped,
  * but it is up to the individual parser method implementations to set the
  * values and remember to un-set them to the previous value after they finish.
  * Forgetting to re-set them to their previous value can cause strange
  * behavior or crashes.
  *
  * For any higher-order-parser that wishes to ignore changes to a field within
  * their wrapped parser method, a common pattern is to save the value of the
  * field before the wrapped parser runs, and then re-set the field. This can
  * be used to backtrack [[index]] after a lookahead parser finishes, or to reset
  * [[failureAggregate]] after an opaque parser finishes.
  *
  * @param input The input to the parsing run, as a String.
  *
  * @param shortFailureMsg The failure message that gets returned when tracing
  *                        is disabled (`traceIndex` == -1). Usually a single
  *                        string or token which is cheap to compute, even if
  *                        it isn't as helpful for debugging as the full traced
  *                        failure message
  *
  * @param failureStack The stack of named P(...) parsers in effect when the
  *                     failure occured; only constructed when tracing is
  *                     enabled via `traceIndex != -1`
  *
  * @param failureAggregate The list of failures that get returned when tracing
  *                         is enabled. This contains every failure message
  *                         that occurred at the exact position of `traceIndex`,
  *                         which is then formatted and presented to the user
  *                         as a better error message for what they could
  *                         possibly do to make their parse succeed
  *
  * @param isSuccess Whether or not the parse is currently successful
  *
  * @param logDepth How many nested `.log` calls are currently surrounding us.
  *                 Used to nicely indent the log output so you can see which
  *                 parsers are nested within which other parsers
  *
  * @param index The current index of the parse
  *
  * @param startIndex Where the parse initially started, so as to allow
  *                   `.result.traced`  to re-create it with tracing enabled.
  *
  * @param cut Has the current parse been prevented from backtracking?
  *
  * @param successValue The currently returned success value
  *
  * @param traceIndex The index we wish to trace if tracing is enabled, else
  *                   -1. Used to find failure messages to aggregate into
  *                   `failureAggregate`
  *
  * @param originalParser The original parsing function we used to start this
  *                       run, so as to allow `.result.traced` to re-create
  *                       it with tracing enabled.
  *
  * @param noDropBuffer Flag that prevents the parser from dropping earlier
  *                     input. Used for the `.!` capture operator that needs
  *                     the input around to return as a string, the `NoCut`
  *                     operator that forces backtracking (regardless of
  *                     internal cuts), or whitespace consumption which
  *                     implicitly backtracks if the parser on the RHS of the
  *                     whitespace fails or consumes 0 characters.
  */
final class ParsingRun[+T](val input: ParserInput,
                           var shortFailureMsg: () => String,
                           var failureStack: List[(String, Int)],
                           var failureAggregate: List[String],
                           var isSuccess: Boolean,
                           var logDepth: Int,
                           var index: Int,
                           val startIndex: Int,
                           var cut: Boolean,
                           var successValue: Any,
                           val traceIndex: Int,
                           var originalParser: ParsingRun[_] => ParsingRun[_],
                           var noDropBuffer: Boolean){
  def read[T](p: ParsingRun[_] => ParsingRun[T] @uncheckedVariance): Parsed[T] = {
    if (originalParser == null) originalParser = p
    p(this).result
  }
  // Use telescoping methods rather than default arguments to try and minimize
  // the amount of bytecode generated at the callsite.
  //
  // Because fastparse inlines aggressively, it is very easy for a user to
  // generate huge methods, so anything we can do to reduce the size of the
  // generated code helps avoid bytecode size blowup

  def aggregateFailure(msg: String) = {
    if (msg != null) failureAggregate = msg :: failureAggregate
  }
  def freshSuccess[V](value: V, msg: => String, index: Int) = {
    if (traceIndex != -1 && traceIndex == this.index) aggregateFailure(msg)
    prepareSuccess(value, index)
  }
  def freshSuccess[V](value: V, msg: => String) = {
    if (traceIndex != -1 && traceIndex == this.index) aggregateFailure(msg)
    prepareSuccess(value, index)
  }
  def prepareSuccess[V](value: V): ParsingRun[V] = {
    isSuccess = true
    successValue = value
    this.asInstanceOf[ParsingRun[V]]
  }
  def prepareSuccess[V](value: V, index: Int): ParsingRun[V] = {
    isSuccess = true
    successValue = value
    this.index = index
    this.asInstanceOf[ParsingRun[V]]
  }
  def prepareSuccess[V](value: V, cut: Boolean): ParsingRun[V] = {
    isSuccess = true
    successValue = value
    this.cut = cut
    this.asInstanceOf[ParsingRun[V]]
  }
  def prepareSuccess[V](value: V, index: Int, cut: Boolean): ParsingRun[V] = {
    isSuccess = true
    successValue = value
    this.index = index
    this.cut = cut
    this.asInstanceOf[ParsingRun[V]]
  }
  def freshFailure(msg: => String): ParsingRun[Nothing] = {
    failureStack = Nil
    val res = prepareFailure(index, cut = this.cut)
    if (traceIndex == -1) this.shortFailureMsg = () => msg
    else if (traceIndex == index) aggregateFailure(msg)
    res
  }
  def freshFailure(msg: => String, startPos: Int): ParsingRun[Nothing] = {
    failureStack = Nil
    val res = prepareFailure(startPos, cut = this.cut)
    if (traceIndex == -1) this.shortFailureMsg = () => msg
    else if (traceIndex == index) aggregateFailure(msg)
    res
  }

  def prepareFailure(index: Int): ParsingRun[Nothing] = {
    isSuccess = false
    this.index = index
    this.asInstanceOf[ParsingRun[Nothing]]
  }
  def prepareFailure(index: Int, cut: Boolean): ParsingRun[Nothing] = {
    isSuccess = false
    this.index = index
    this.cut = cut
    this.asInstanceOf[ParsingRun[Nothing]]
  }

  def checkForDrop() = !noDropBuffer && cut

  def result: Parsed[T] = {
    if (isSuccess) Parsed.Success(successValue.asInstanceOf[T], index)
    else {
      val stack =
        if (failureAggregate.isEmpty) {
          if (shortFailureMsg == null) List("" -> index)
          else List(shortFailureMsg() -> index)
        }else{

          val tokens = failureAggregate.distinct.reverse
          val combined =
            if (tokens.length == 1) tokens.mkString(" | ")
            else tokens.mkString("(", " | ", ")")
            (combined -> index) :: failureStack.reverse
        }
      Parsed.Failure(
        stack,
        index,
        Parsed.Extra(input, startIndex, index, originalParser)
      )
    }
  }
}

object ParsingRun{
  def current(implicit i: ParsingRun[Any]): ParsingRun[Any] = i
}