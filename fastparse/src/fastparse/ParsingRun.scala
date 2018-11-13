package fastparse

import fastparse.internal.{Instrument, Lazy, Msgs, Util}

/**
  * Models an in-progress parsing run; contains all the mutable state that may
  * be necessary during the parse, in order to avoid the individual parsers
  * needing to perform their own allocations and instantiations, and greatly
  * improving performance
  *
  * There are a few patterns that let us program with these mutable variables
  * in a sort-of-pure-functional way:
  *
  * - If a parser that wishes to ignore changes to a field within their child
  *   parsers, a common pattern is to save the value of the field before the
  *   wrapped parser runs, and then re-set the field. e.g. this can be used to
  *   backtrack [[index]] after a lookahead parser finishes
  *
  * - If a parser wants to read the value of the field "returned" by multiple
  *   child parsers, make sure to read the field into a local variable after
  *   each child parser is complete to make sure the value you want from an
  *   earlier child isn't stomped over by a later child
  *
  *   In general, for a parser to "return" a value in a mutable field, it is
  *   sufficient to simply set the value of that field before returning. It
  *   is the parent-parser's responsibility to make sure it reads out the value
  *   of the field to a local variable before running another child parser that
  *   will over-write the mutable field
  *
  * @param input            The input to the parsing run, as a [[ParserInput]].
  * @param startIndex       Where the parse initially started, so as to allow
  *                         `.result.traced`  to re-create it with tracing enabled.
  * @param originalParser   The original parsing function we used to start this
  *                         run, so as to allow `.result.traced` to re-create
  *                         it with tracing enabled.
  * @param traceIndex       The index we wish to trace if tracing is enabled, else
  *                         -1. Used to find failure messages to aggregate into
  *                         `failureTerminalAggregate`
  * @param instrument       Callbacks that can be injected before/after every
  *                         `P(...)` parser.
  * @param failureTerminalAggregate When tracing is enabled, this collects up all the
  *                         upper-most failures that happen at [[traceIndex]]
  *                         (in [[Lazy]] wrappers) so they can be shown to the
  *                         user at end-of-parse as suggestions for what could
  *                         make the parse succeed. For terminal parsers like
  *                         [[LiteralStr]], it just aggregate's the string
  *                         representation. For composite parsers like `a ~ b`
  *                         or `!a` which may fail at [[traceIndex]] even
  *                         without any of their wrapped terminal parsers
  *                         failing there, it makes use of the
  *                         [[shortParserMsg]] as the string representation of
  *                         the composite parser.
  * @param shortParserMsg   When tracing is enabled, this contains string
  *                         representation of the last parser to run. Since
  *                         parsers aren't really objects, we piece together
  *                         the string in the parser body and return store it
  *                         here, and an enclosing parser may fetch it back
  *                         out to help build its own string representation.
  *                         If the last parser started before the `traceIndex`,
  *                         we only aggregate the portion of the parser msg
  *                         that takes place after `traceIndex`
  * @param failureStack     The stack of named `P(...)` parsers in effect when
  *                         the failure occured; only constructed when tracing
  *                         is enabled via `traceIndex != -1`
  * @param isSuccess        Whether or not the parse is currently successful
  * @param logDepth         How many nested `.log` calls are currently surrounding us.
  *                         Used to nicely indent the log output so you can see which
  *                         parsers are nested within which other parsers; -1 means
  *                         logging is disabled
  * @param index            The current index of the parse
  * @param cut              Has the current parse been prevented from backtracking?
  *                         This field starts as `true` at top-level, since there
  *                         is nowhere to backtrack to. Upon entering a parser
  *                         that can backtrack, such as `|` or `.?` or `.rep`,
  *                         it is set to `false`, and re-set to `true` upon
  *                         encountering a `./` or `~/` cut operator that prevents
  *                         backtracking.
  * @param successValue     The currently returned success value
  * @param verboseFailures  Whether or not we are currently collecting [[lastFailureMsg]]s;
  *                         defaults to false, unless
  *                         [[traceIndex]] is set OR we are inside a [[LogByNameOps.log]]
  *                         call which necessitates tracing in order to print out
  *                         failure traces during logging
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
                           val startIndex: Int,
                           val originalParser: ParsingRun[_] => ParsingRun[_],
                           val traceIndex: Int,
                           val instrument: Instrument,
                           // Mutable vars below:
                           var failureTerminalAggregate: Msgs,
                           var failureGroupAggregate: Msgs,
                           var shortParserMsg: Msgs,
                           var lastFailureMsg: Msgs,
                           var failureStack: List[(String, Int)],
                           var isSuccess: Boolean,
                           var logDepth: Int,
                           var index: Int,
                           var cut: Boolean,
                           var successValue: Any,
                           var verboseFailures: Boolean,
                           var noDropBuffer: Boolean){


  def aggregateMsg(startIndex: Int,
                   msgToAggregate: Msgs,
                   startGroup: Msgs): Unit = {
    aggregateMsg(startIndex, msgToAggregate, msgToAggregate, startGroup)
  }

  def aggregateMsg(startIndex: Int,
                   msgToSet: () => String,
                   msgToAggregate: Msgs,
                   startGroup: Msgs): Unit = {
    aggregateMsg(startIndex, Msgs(List(new Lazy(msgToSet))), msgToAggregate, startGroup)
  }
  def aggregateMsg(startIndex: Int,
                   msgToSet: Msgs,
                   msgToAggregate: Msgs,
                   startGroup: Msgs,
                   force: Boolean = false): Unit = {
    // We only aggregate the msg under the following conditions:
    //
    // - The current index matches the traceIndex; all messages at other
    //   indices are ignored
    //
    // - We are either in a backtracking (no-cut) failure, or we are in
    //   some kind of success.
    //   - Non-backtracking failures we do not log, because they get naturally
    //     logged as part of lastFailureMsg
    //   - Successes we *do* log, because often aggregateMsg gets called from
    //     .rep and .? bodies, where the parse succeeds even though the last
    //     aborted inner-parse failed (and needs to be aggregated)
    if (!cut || isSuccess || force) {
      failureGroupAggregate = msgToAggregate ::: startGroup
    }

    // Only use the msgToSet if our start index is greater than the traceIndex.
    // If it is less, that means the `shortParserMsg` we are setting is only used
    // for group aggregation and not returned on it's own, so we return the
    // possibly-truncated `msgToAggregate` to use in any surrounding aggregates
    if (startIndex >= traceIndex) setMsg(startIndex, msgToSet)
    else setMsg(Int.MaxValue, msgToAggregate)
  }

  def aggregateTerminal(startIndex: Int, f: () => String): Unit = {
    val f2 = new Lazy(f)
    if (!isSuccess){
      if (index == traceIndex) failureTerminalAggregate ::= f2
      if (lastFailureMsg == null) lastFailureMsg = Msgs(List(f2))
    }

    shortParserMsg = Msgs(if (startIndex >= traceIndex) List(f2) else Nil)
  }
  def setMsg(startIndex: Int, f: () => String): Unit = {
    val f2 = new Lazy(f)
    if (!isSuccess && lastFailureMsg == null) lastFailureMsg = Msgs(List(f2))
    shortParserMsg = Msgs(if (startIndex >= traceIndex) List(f2) else Nil)
  }
  def setMsg(startIndex: Int, f: Msgs): Unit = {
    if (!isSuccess && lastFailureMsg == null) lastFailureMsg = f
    shortParserMsg =  if (startIndex >= traceIndex) f else Msgs(Nil)
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

  def freshSuccessUnit(): ParsingRun[Unit] = {
    isSuccess = true
    successValue = ()
    this.asInstanceOf[ParsingRun[Unit]]
  }

  def freshSuccessUnit(index: Int): ParsingRun[Unit] = {
    isSuccess = true
    successValue = ()

    this.index = index
    this.asInstanceOf[ParsingRun[Unit]]
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
    if (verboseFailures){
      lastFailureMsg = null
      failureStack = Nil
    }
    isSuccess = false
    this.asInstanceOf[ParsingRun[Nothing]]
  }

  def freshFailure(startPos: Int): ParsingRun[Nothing] = {
    if (verboseFailures) {
      lastFailureMsg = null
      failureStack = Nil
    }
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
}

object ParsingRun{
  def current(implicit i: ParsingRun[Any]): ParsingRun[Any] = i
}