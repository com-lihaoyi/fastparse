package fasterparser

import scala.annotation.unchecked.uncheckedVariance


/**
  * Models an in-progress parsing run; contains all the mutable state that may
  * be necessary during the parse, in order to avoid the individual parsers
  * needing to perform their own allocations and instantiations.
  *
  * @param input The input to the parsing run, as a String.
  * @param failureStack
  * @param shortFailureMsg The failure message that gets returned when tracing
  *                        is disabled (`traceIndex` == -1). Usually a single
  *                        string or token which is cheap to compute, even if
  *                        it isn't as helpful for debugging as the full traced
  *                        failure message
  * @param failureAggregate The list of failures that get returned when tracing
  *                         is enabled. This contains every failure message
  *                         that occurred at the exact position of `traceIndex`,
  *                         which is then formatted and presented to the user
  *                         as a better error message for what they could
  *                         possibly do to make their parse succeed
  * @param isSuccess Whether or not the parse is currently successful
  * @param logDepth How many nested `.log` calls are currently surrounding us.
  *                 Used to nicely indent the log output so you can see which
  *                 parsers are nested within which other parsers
  * @param index The current index of the parse
  * @param startIndex Where the parse initially started, so as to allow
  *                   `.result.traced`  to re-create it with tracing enabled.
  * @param cut Has the current parse been prevented from backtracking?
  * @param successValue The currently returned success value
  * @param traceIndex The index we wish to trace if tracing is enabled, else
  *                   -1. Used to find failure messages to aggregate into
  *                   `failureAggregate`
  * @param originalParser The original parsing function we used to start this
  *                       run, so as to allow `.result.traced` to re-create
  *                       it with tracing enabled.
  */
class Parse[+T](val input: String,
                var failureStack: List[(String, Int)],
                var shortFailureMsg: () => String,
                var failureAggregate: List[String],
                var isSuccess: Boolean,
                var logDepth: Int,
                var index: Int,
                val startIndex: Int,
                var cut: Boolean,
                var successValue: Any,
                val traceIndex: Int,
                var originalParser: Parse[_] => Parse[_]){
  def read[T](p: Parse[_] => Parse[T] @uncheckedVariance): Result[T] = {
    if (originalParser == null) originalParser = p
    p(this).result
  }
  // Use telescoping methods rather than default arguments to try and minimize
  // the amount of bytecode generated at the callsite.
  //
  // Because FasterParser inlines aggressively, it is very easy for a user to
  // generate huge methods, so anything we can do to reduce the size of the
  // generated code helps avoid bytecode size blowup

  def aggregateFailure(msg: String) = {
    if (msg != null) failureAggregate = msg :: failureAggregate
  }
  def freshSuccess[V](value: V, msg: => String, index: Int) = {
    if (traceIndex != -1 && traceIndex == this.index) aggregateFailure(msg)
    prepareSuccess(value, index, cut = false)
  }
  def freshSuccess[V](value: V, msg: => String) = {
    if (traceIndex != -1 && traceIndex == this.index) aggregateFailure(msg)
    prepareSuccess(value, index, cut = false)
  }
  def prepareSuccess[V](value: V): Parse[V] = prepareSuccess(value, index, cut)
  def prepareSuccess[V](value: V, index: Int): Parse[V] = prepareSuccess(value, index, cut)
  def prepareSuccess[V](value: V, cut: Boolean): Parse[V] = prepareSuccess(value, index, cut)
  def prepareSuccess[V](value: V, index: Int, cut: Boolean): Parse[V] = {
    isSuccess = true
    successValue = value
    this.index = index
    this.cut = cut
    this.asInstanceOf[Parse[V]]
  }
  def freshFailure(msg: => String): Parse[Nothing] = {
    failureStack = Nil
    val res = prepareFailure(index, cut = false)
    if (traceIndex == -1) this.shortFailureMsg = () => msg
    else if (traceIndex == index) aggregateFailure(msg)
    res
  }
  def freshFailure(msg: => String, startPos: Int): Parse[Nothing] = {
    failureStack = Nil
    val res = prepareFailure(startPos, cut = false)
    if (traceIndex == -1) this.shortFailureMsg = () => msg
    else if (traceIndex == index) aggregateFailure(msg)
    res
  }

  def prepareFailure(index: Int): Parse[Nothing] = prepareFailure(index, cut)
  def prepareFailure(index: Int, cut: Boolean): Parse[Nothing] = {
    isSuccess = false
    this.index = index
    this.cut = cut
    this.asInstanceOf[Parse[Nothing]]
  }

  def result: Result[T] = {
    if (isSuccess) Result.Success(successValue.asInstanceOf[T], index)
    else {
      val msg =
        if (failureAggregate.isEmpty) Option(shortFailureMsg).fold("")(_())
        else {
          val tokens = failureAggregate.distinct.reverse
          if (tokens.length == 1) tokens.mkString(" | ")
          else tokens.mkString("(", " | ", ")")
        }
      Result.Failure(
        index,
        (msg -> index) :: failureStack.reverse,
        Result.Extra(input, startIndex, index, originalParser)
      )
    }
  }
}
object Parse{
  def apply()(implicit i: Parse[Any]): Parse[Any] = i
  def apply(input: String, startIndex: Int = 0, traceIndex: Int = -1) = new Parse(
    input = input,
    failureStack = List.empty,
    shortFailureMsg = null,
    failureAggregate = List.empty,
    isSuccess = true,
    logDepth = 0,
    startIndex, startIndex, false, (), traceIndex, null
  )
}
