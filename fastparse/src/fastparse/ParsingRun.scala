package fastparse

/**
  * Models an in-progress parsing run; contains all the mutable state that may
  * be necessary during the parse, in order to avoid the individual parsers
  * needing to perform their own allocations and instantiations.
  *
  * For any higher-order-parser that wishes to ignore changes to a field within
  * their wrapped parser method, a common pattern is to save the value of the
  * field before the wrapped parser runs, and then re-set the field. e.g. this
  * can be used to backtrack [[index]] after a lookahead parser finishes
  *
  * @param input            The input to the parsing run, as a [[ParserInput]].
  *
  * @param shortFailureMsg  The failure message that gets returned when tracing
  *                         is disabled (`traceIndex` == -1). Usually a single
  *                         string or token which is cheap to compute, even if
  *                         it isn't as helpful for debugging as the full traced
  *                         failure message
  *
  * @param failureStack     The stack of named P(...) parsers in effect when the
  *                         failure occured; only constructed when tracing is
  *                         enabled via `traceIndex != -1`

  *
  * @param isSuccess        Whether or not the parse is currently successful
  *
  * @param logDepth         How many nested `.log` calls are currently surrounding us.
  *                         Used to nicely indent the log output so you can see which
  *                         parsers are nested within which other parsers; -1 means
  *                         logging is disabled
  *
  * @param index            The current index of the parse
  *
  * @param startIndex       Where the parse initially started, so as to allow
  *                         `.result.traced`  to re-create it with tracing enabled.
  *
  * @param cut              Has the current parse been prevented from backtracking?
  *                         This field starts as `true` at top-level, since there
  *                         is nowhere to backtrack to. Upon entering a parser
  *                         that can backtrack, such as `|` or `.?` or `.rep`,
  *                         it is set to `false`, and re-set to `true` upon
  *                         encountering a `./` or `~/` cut operator that prevents
  *                         backtracking.
  *
  * @param successValue     The currently returned success value
  *
  * @param traceIndex       The index we wish to trace if tracing is enabled, else
  *                         -1. Used to find failure messages to aggregate into
  *                         `failureAggregate`
  *
  * @param originalParser   The original parsing function we used to start this
  *                         run, so as to allow `.result.traced` to re-create
  *                         it with tracing enabled.
  *
  * @param noDropBuffer     Flag that prevents the parser from dropping earlier
  *                         input. Used for the `.!` capture operator that needs
  *                         the input around to return as a string, the `NoCut`
  *                         operator that forces backtracking (regardless of
  *                         internal cuts), or whitespace consumption which
  *                         implicitly backtracks if the parser on the RHS of
  *                         the whitespace fails or consumes 0 characters. The
  *                         value for this field is lexically scoped, but it is
  *                         up to the individual parser method implementations
  *                         to set the values and remember to un-set it to
  *                         the previous value after they finish. Forgetting to
  *                         re-set it to the previous value can cause strange
  *                         behavior or crashes.
  */
final class ParsingRun[+T](val input: ParserInput,
                           var failureAggregate: List[() => String],
                           var shortFailureMsg: () => String,
                           var failureStack: List[(String, Int)],
                           var isSuccess: Boolean,
                           var logDepth: Int,
                           var index: Int,
                           val startIndex: Int,
                           var cut: Boolean,
                           var successValue: Any,
                           val traceIndex: Int,
                           val originalParser: ParsingRun[_] => ParsingRun[_],
                           var noDropBuffer: Boolean,
                           val instrument: ParsingRun.Instrument){

  val tracingEnabled = traceIndex != -1

  def setMsg(f: () => String): Unit = {
    shortFailureMsg = f
  }

  def aggregateMsg(f: () => String): Unit = {
    if (!isSuccess && index == traceIndex) failureAggregate ::= f
    shortFailureMsg = f
  }

  // Use telescoping methods rather than default arguments to try and minimize
  // the amount of bytecode generated at the callsite.
  //
  // Because fastparse inlines aggressively, it is very easy for a user to
  // generate huge methods, so anything we can do to reduce the size of the
  // generated code helps avoid bytecode size blowup
  def freshSuccess[V](value: V): ParsingRun[V] = {
    isSuccess = true
    successValue = value
    this.asInstanceOf[ParsingRun[V]]
  }

  def freshSuccess[V](value: V, index: Int): ParsingRun[V] = {
    isSuccess = true
    successValue = value

    this.index = index
    this.asInstanceOf[ParsingRun[V]]
  }

  def freshSuccess[V](value: V, cut: Boolean): ParsingRun[V] = {
    isSuccess = true
    successValue = value
    this.cut = cut
    this.asInstanceOf[ParsingRun[V]]
  }

  def freshSuccess[V](value: V, index: Int, cut: Boolean): ParsingRun[V] = {
    isSuccess = true
    successValue = value
    this.index = index
    this.cut = cut
    this.asInstanceOf[ParsingRun[V]]
  }

  def freshFailure(): ParsingRun[Nothing] = {
    failureStack = Nil
    isSuccess = false
    this.asInstanceOf[ParsingRun[Nothing]]
  }

  def freshFailure(startPos: Int): ParsingRun[Nothing] = {
    failureStack = Nil
    isSuccess = false
    index = startPos
    this.asInstanceOf[ParsingRun[Nothing]]
  }


  def augmentFailure(index: Int): ParsingRun[Nothing] = {
    this.index = index
    this.asInstanceOf[ParsingRun[Nothing]]
  }

  def augmentFailure(index: Int, cut: Boolean): ParsingRun[Nothing] = {
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

          val tokens = failureAggregate.reverse.map(_()).distinct
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
  trait Instrument{
    def beforeParse(parser: String, index: Int): Unit
    def afterParse(parser: String, index: Int, success: Boolean): Unit
  }
  def current(implicit i: ParsingRun[Any]): ParsingRun[Any] = i
}