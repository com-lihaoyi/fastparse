import fastparse.internal._
import fastparse.internal.{Instrument, Logger}

import language.experimental.macros

package object fastparse {
  /**
    * Parses the given input [[ParserInput]] using the given parser and returns
    * a [[Parsed]] result containing the success value or failure metadata.
    *
    * Can take either a [[String]], an [[Iterator]] or strings or a
    * [[fastparse.ParserInput]] object
    *
    * @param input the input to parse
    *
    * @param parser the parser method to use to parse the input
    *
    * @param verboseFailures enable this to show a more detailed error message
    *                        if a parser fails, without needing to run
    *                        `.traced.trace`. Defaults to `false` as it slows
    *                        down parsing considerably
    *
    * @param startIndex where in the input to start parsing
    *
    * @param instrument Callbacks that get run before and after every named
    *                   `P(...)` parser
    *
    *
    */
  def parse[T](input: ParserInputSource,
               parser: P[_] => P[T],
               verboseFailures: Boolean = false,
               startIndex: Int = 0,
               instrument: Instrument = null): Parsed[T] = {
    Parsed.fromParsingRun(input.parseThrough(parseInputRaw[T](
      _,
      parser,
      verboseFailures,
      startIndex,
      -1,
      instrument
    )))
  }

  def parseInputRaw[T](input: ParserInput,
                       parser: P[_] => P[T],
                       verboseFailures: Boolean = false,
                       startIndex: Int = 0,
                       traceIndex: Int = -1,
                       instrument: Instrument = null,
                       enableLogging: Boolean = true): ParsingRun[T] = parser(new ParsingRun(
    input = input,
    startIndex = startIndex,
    originalParser = parser,
    traceIndex = traceIndex,
    instrument = instrument,
    failureTerminalAggregate = Msgs.empty,
    failureGroupAggregate = Msgs.empty,
    shortParserMsg = Msgs.empty,
    lastFailureMsg = null,
    failureStack = List.empty,
    isSuccess = true,
    logDepth = if (enableLogging) 0 else -1,
    startIndex,
    true,
    (),
    verboseFailures,
    false,
    collection.mutable.Map.empty
  ))


  /**
    * Shorthand alias for [[ParsingRun]]; this is both the parameter-to and the
    * return type for all Fastparse's parsing methods.
    *
    * @tparam T is the type of the value returned by the parser method on success
    */
  type P[+T] = ParsingRun[T]
  val P = ParsingRun
  /**
    * Shorthand for `P[Unit]`
    */
  type P0 = P[Unit]

  /**
    * Delimits a named parser. This name will appear in the parser failure
    * messages and stack traces, and by default is taken from the name of the
    * enclosing method.
    */
  def P[T](t: P[T])(implicit name: sourcecode.Name, ctx: P[_]): P[T] = macro MacroImpls.pMacro[T]

  /**
    * Parses an exact string value.
    */
  implicit def LiteralStr(s: String)(implicit ctx: P[Any]): P[Unit] = macro MacroImpls.literalStrMacro

  /**
    * Parses a string value case-insensitively
    */
  def IgnoreCase(s: String)(implicit ctx: P[Any]): P[Unit] = {
    val startIndex = ctx.index
    val res =
      if (Util.startsWithIgnoreCase(ctx.input, s, ctx.index)) ctx.freshSuccessUnit(ctx.index + s.length)
      else ctx.freshFailure().asInstanceOf[P[Unit]]
    if (ctx.verboseFailures) ctx.aggregateTerminal(startIndex, () => Util.literalize(s))
    res
  }

  /**
    * Provides [[EagerOps]] extension methods on [[String]]
    */
  implicit def EagerOpsStr(parse0: String)(implicit ctx: P[Any]): fastparse.EagerOps[Unit] = macro MacroImpls.eagerOpsStrMacro


  /**
    * Provides [[EagerOps]] extension methods on [[P]]]
    */
  implicit class EagerOps[T](val parse0: P[T]) extends AnyVal{
    /**
      * Plain cut operator. Runs the parser, and if it succeeds, backtracking
      * past that point is now prohibited
      */
    def /(implicit ctx: P[_]): P[T] = macro MacroImpls.cutMacro[T]

    /**
      * Sequence-with-cut operator. Runs two parsers one after the other,
      * with optional whitespace in between. If the first parser completes
      * successfully, backtracking is now prohibited. If both parsers
      * return a value, this returns a tuple.
      */
    def ~/[V, R](other: P[V])
                (implicit s: Implicits.Sequencer[T, V, R],
                 whitespace: P[Any] => P[Unit],
                 ctx: P[_]): P[R] = macro MacroImpls.parsedSequenceCut[T, V, R]

    /**
      * Sequence operator. Runs two parsers one after the other,
      * with optional whitespace in between.If both parsers
      * return a value, this returns a tuple.
      */
    def ~[V, R](other:  P[V])
               (implicit s: Implicits.Sequencer[T, V, R],
                whitespace: P[Any] => P[Unit],
                ctx: P[_]): P[R] = macro MacroImpls.parsedSequence[T, V, R]

    /**
      * Raw sequence-with-cut operator. Runs two parsers one after the other,
      * *without* whitespace in between. If the first parser completes
      * successfully, backtracking is no longer prohibited. If both parsers
      * return a value, this returns a tuple.
      */
    def ~~/[V, R](other: P[V])
                 (implicit s: Implicits.Sequencer[T, V, R],
                  ctx: P[_]): P[R] = macro MacroImpls.parsedSequenceCut1[T, V, R]

    /**
      * Raw sequence operator. Runs two parsers one after the other,
      * *without* whitespace in between. If both parsers return a value,
      * this returns a tuple.
      */
    def ~~[V, R](other: P[V])
                (implicit s: Implicits.Sequencer[T, V, R],
                 ctx: P[_]): P[R] = macro MacroImpls.parsedSequence1[T, V, R]

    /**
      * Transforms the result of this parser using the given function. Useful
      * for turning the [[String]]s captured by [[!]] and the tuples built
      * by [[~]] into your own case classes or data structure
      */
    def map[V](f: T => V): P[V] = macro MacroImpls.mapMacro[T, V]

    /**
      * Tests the output of the parser with a given predicate, failing the
      * parse if the predicate returns false. Useful for doing local validation
      * on bits and pieces of your parsed output
      */
    def filter(f: T => Boolean)
              (implicit ctx: P[Any]): P[T] = macro MacroImpls.filterMacro[T]
    /**
      * Transforms the result of this parser using the given function into a
      * new parser which is applied (after whitespace). Useful for doing
      * dependent parsing, e.g. when parsing JSON you may first parse a
      * character to see if it's a `[`, `{`, or `"`, and then deciding whether
      * you next want to parse an array, dictionary or string.
      */
    def flatMap[V](f: T => P[V])
                  (implicit whitespace: P[Any] => P[Unit]): P[V] = macro MacroImpls.flatMapMacro[T, V]
    /**
      * Transforms the result of this parser using the given function into a
      * new parser which is applied (without consuming whitespace). Useful for
      * doing dependent parsing, e.g. when parsing JSON you may first parse a
      * character to see if it's a `[`, `{`, or `"`, and then deciding whether
      * you next want to parse an array, dictionary or string.
      */
    def flatMapX[V](f: T => P[V]): P[V] = macro MacroImpls.flatMapXMacro[T, V]

    /**
      * Either-or operator: tries to parse the left-hand-side, and if that
      * fails it backtracks and tries to pass the right-hand-side. Can be
      * chained more than once to parse larger numbers of alternatives.
      */
    def |[V >: T](other: P[V])
                 (implicit ctx: P[Any]): P[V] = macro MacroImpls.eitherMacro[T, V]

    /**
      * Capture operator; makes the parser return the span of input it parsed
      * as a [[String]], which can then be processed further using [[~]],
      * [[map]] or [[flatMapX]]
      */
    def !(implicit ctx: P[Any]): P[String] = macro MacroImpls.captureMacro

    /**
      * Optional operator. Parses the given input to wrap it in a `Some`, but
      * if parsing fails backtracks and returns `None`
      */
    def ?[V](implicit optioner: Implicits.Optioner[T, V],
             ctx: P[Any]): P[V] = macro MacroImpls.optionMacro[T, V]
  }

  /**
    * Provides [[ByNameOps]] extension methods on [[String]]s
    */
  implicit def ByNameOpsStr(parse0: String)(implicit ctx: P[Any]): fastparse.ByNameOps[Unit] =
  macro MacroImpls.byNameOpsStrMacro
  /**
    * Provides [[ByNameOps]] extension methods on [[P]]s
    */
  implicit def ByNameOps[T](parse0: => P[T]) = new ByNameOps(() => parse0)
  class ByNameOps[T](val parse0: () => P[T]) extends AnyVal{
    /**
      * Repeat operator; runs the LHS parser 0 or more times separated by the
      * given whitespace (in implicit scope), and returns
      * a `Seq[T]` of the parsed values. On failure, backtracks to the starting
      * index of the last run.
      */
    def rep[V](implicit repeater: Implicits.Repeater[T, V],
               whitespace: P[_] => P[Unit],
               ctx: P[Any]): P[V] = macro MacroRepImpls.repXMacro1ws[T, V]
    /**
      * Repeat operator; runs the LHS parser at least `min` to at most `max`
      * times separated by the given whitespace (in implicit scope) and
      * separator `sep`, and returns a `Seq[T]` of the parsed values. On
      * failure, backtracks to the starting index of the last run.
      *
      * The convenience parameter `exactly` is provided to set both `min` and
      * `max` to the same value.
      */
    def rep[V](min: Int = 0,
               sep: => P[_] = null,
               max: Int = Int.MaxValue,
               exactly: Int = -1)
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: P[_] => P[Unit],
               ctx: P[Any]): P[V] =
      new RepImpls[T](parse0).rep[V](min, sep, max, exactly)

    /**
      * Repeat operator; runs the LHS parser at least `min`
      * times separated by the given whitespace (in implicit scope) and
      * separator `sep`, and returns a `Seq[T]` of the parsed values. On
      * failure, backtracks to the starting index of the last run.
      */
    def rep[V](min: Int,
               sep: => P[_])
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: P[_] => P[Unit],
               ctx: P[Any]): P[V] =
      new RepImpls[T](parse0).rep[V](min, sep)

    /**
      * Repeat operator; runs the LHS parser at least `min`
      * times separated by the given whitespace (in implicit scope),
      * and returns a `Seq[T]` of the parsed values. On
      * failure, backtracks to the starting index of the last run.
      */
    def rep[V](min: Int)
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: P[_] => P[Unit],
               ctx: P[Any]): P[V] =
    macro MacroRepImpls.repXMacro2ws[T, V]

    /**
      * Raw repeat operator; runs the LHS parser 0 or more times *without*
      * any whitespace in between, and returns
      * a `Seq[T]` of the parsed values. On failure, backtracks to the starting
      * index of the last run.
      */
    def repX[V](implicit repeater: Implicits.Repeater[T, V],
                ctx: P[Any]): P[V] =
    macro MacroRepImpls.repXMacro1[T, V]

    /**
      * Raw repeat operator; runs the LHS parser at least `min` to at most `max`
      * times separated by the
      * separator `sep` *without* any whitespace in between, and returns a `Seq[T]` of the parsed values. On
      * failure, backtracks to the starting index of the last run.
      *
      * The convenience parameter `exactly` is provided to set both `min` and
      * `max` to the same value.
      */
    def repX[V](min: Int = 0,
                sep: => P[_] = null,
                max: Int = Int.MaxValue,
                exactly: Int = -1)
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: P[Any]): P[V] =
      new RepImpls[T](parse0).repX[V](min, sep, max, exactly)

    /**
      * Raw repeat operator; runs the LHS parser at least `min`
      * times separated by the separator `sep`, *without* any whitespace
      * in between, and returns a `Seq[T]` of the parsed values. On
      * failure, backtracks to the starting index of the last run.
      */
    def repX[V](min: Int,
                sep: => P[_])
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: P[Any]): P[V] =
      new RepImpls[T](parse0).repX[V](min, sep)

    /**
      * Raw repeat operator; runs the LHS parser at least `min`
      * times *without* any whitespace in between,
      * and returns a `Seq[T]` of the parsed values. On
      * failure, backtracks to the starting index of the last run.
      */
    def repX[V](min: Int)
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: P[Any]): P[V] =
    macro MacroRepImpls.repXMacro2[T, V]

    /**
      * Hides the internals of the given parser when it fails, such that it
      * only succeeds completely or fails completely, and none of it's internal
      * parsers end up in the failure traces or failure stack to be displayed
      * to the user.
      */
    def opaque(msg: String)(implicit ctx: P[Any]): P[T] = {
      val oldIndex = ctx.index

      val res = parse0()

      val res2 =
        if (res.isSuccess) ctx.freshSuccess(ctx.successValue)
        else ctx.freshFailure(oldIndex)

      if (ctx.verboseFailures) ctx.aggregateTerminal(oldIndex, () => msg)

      res2.asInstanceOf[P[T]]
    }

    /**
      * Negative lookahead operator: succeeds if the wrapped parser fails and
      * fails if the wrapped parser succeeds. In all cases, it ends up
      * consuming zero characters.
      */
    def unary_!(implicit ctx: P[Any]) : P[Unit] = {
      val startPos = ctx.index
      val startCut = ctx.cut
      val oldNoCut = ctx.noDropBuffer
      ctx.noDropBuffer = true
      val startTerminals = ctx.failureTerminalAggregate
      parse0()
      ctx.noDropBuffer = oldNoCut
      val msg = ctx.shortParserMsg

      val res =
        if (ctx.isSuccess) ctx.freshFailure(startPos)
        else ctx.freshSuccessUnit(startPos)

      if (ctx.verboseFailures) {
        ctx.failureTerminalAggregate = startTerminals
        ctx.failureGroupAggregate = Msgs.empty
        ctx.setMsg(startPos, () => "!" + msg.render)
      }
      res.cut = startCut
      res
    }

  }

  /**
    * Provides logging-related [[LogByNameOps]] implicits on [[String]].
    */
  implicit def LogOpsStr(parse0: String)
                        (implicit ctx: P[Any]): fastparse.LogByNameOps[Unit] =
    macro MacroImpls.logOpsStrMacro
  /**
    * Separated out from [[ByNameOps]] because `.log` isn't easy to make an
    * [[AnyVal]] extension method, but it doesn't matter since `.log` calls
    * are only for use in development while the other [[ByNameOps]] operators
    * are more performance-sensitive
    */
  implicit class  LogByNameOps[T](parse0: => P[T])(implicit ctx: P[_]) {
    /**
      * Wraps a parser to log when it succeeds and fails, and at what index.
      * Useful for seeing what is going on within your parser. Nicely indents
      * the logs for easy reading
      */
    def log(implicit name: sourcecode.Name, logger: Logger = Logger.stdout): P[T] = {
      if (ctx.logDepth == -1) parse0
      else {
        val msg = name.value
        val output = logger.f
        val indent = "  " * ctx.logDepth

        output(s"$indent+$msg:${ctx.input.prettyIndex(ctx.index)}${if (ctx.cut) ", cut" else ""}")
        val depth = ctx.logDepth
        ctx.logDepth += 1
        val startIndex = ctx.index
        val oldverboseFailures = ctx.verboseFailures
        ctx.verboseFailures = true
        parse0
        ctx.verboseFailures = oldverboseFailures
        ctx.logDepth = depth
        val prettyIndex = ctx.input.prettyIndex(ctx.index)
        val strRes = if (ctx.isSuccess) {
          s"Success($prettyIndex${if (ctx.cut) ", cut" else ""})"
        } else {
          val trace = Parsed.Failure.formatStack(
            ctx.input,
            ctx.failureStack ++ Seq(ctx.lastFailureMsg.render -> ctx.index)
          )
          val trailing = ctx.input match {
            case c: IndexedParserInput => Parsed.Failure.formatTrailing(ctx.input, startIndex)
            case _ => ""
          }
          s"Failure($trace ...$trailing${if (ctx.cut) ", cut" else ""})"
        }
        output(s"$indent-$msg:${ctx.input.prettyIndex(startIndex)}:$strRes")
        //        output(s"$indent-$msg:${repr.prettyIndex(cfg.input, index)}:$strRes")
        ctx.asInstanceOf[P[T]]
      }
    }

    /**
      * Prints the given message, nicely indented, after the wrapped parser finishes
      */
    def logAfter(msg: => Any)(implicit logger: Logger = Logger.stdout): P[T] = {
      val indent = "  " * ctx.logDepth
      val res = parse0
      if (ctx.logDepth != -1) logger.f(indent + msg)
      res
    }

    /**
      * Prints the given message, nicely indented, before the wrapped parser starts
      */
    def logBefore(msg: => Any)(implicit logger: Logger = Logger.stdout): P[T] = {
      val indent = "  " * ctx.logDepth
      if (ctx.logDepth != -1) logger.f(indent + msg)
      val res = parse0
      res
    }
  }

  /**
    * Positive lookahead operator: succeeds if the wrapped parser succeeds and
    * fails if the wrapped parser fails, but in all cases consumes zero
    * characters.
    */
  def &(parse: => P[_])(implicit ctx: P[_]): P[Unit] = {

    val startPos = ctx.index
    val startCut = ctx.cut
    val oldNoCut = ctx.noDropBuffer
    ctx.noDropBuffer = true
    parse
    ctx.noDropBuffer = oldNoCut
    val msg = ctx.shortParserMsg

    val res =
      if (ctx.isSuccess) ctx.freshSuccessUnit(startPos)
      else ctx.asInstanceOf[P[Unit]]
    if (ctx.verboseFailures) {
      ctx.failureGroupAggregate = Msgs.empty
      ctx.setMsg(startPos, () =>
        msg match{
          case Seq(x) => s"&(${msg.render})"
          case xs => s"&${msg.render}"
        }
      )
    }
    res.cut = startCut
    res
  }

  /**
    * Parser that is only successful at the end of the input. Useful to ensure
    * your parser parses the whole file.
    */
  def End(implicit ctx: P[_]): P[Unit] = {
    val startIndex = ctx.index
    val res =
      if (!ctx.input.isReachable(startIndex)) ctx.freshSuccessUnit()
      else ctx.freshFailure().asInstanceOf[P[Unit]]
    if (ctx.verboseFailures) ctx.aggregateTerminal(startIndex, () => "end-of-input")
    res

  }
  /**
    * Parser that is only successful at the start of the input.
    */
  def Start(implicit ctx: P[_]): P[Unit] = {
    val startIndex = ctx.index
    val res =
      if (startIndex == 0) ctx.freshSuccessUnit()
      else ctx.freshFailure().asInstanceOf[P[Unit]]
    if (ctx.verboseFailures) ctx.aggregateTerminal(startIndex, () => "start-of-input")
    res
  }

  /**
    * Wraps a parser and ensures that none of the parsers within it leave
    * failure traces in failureTerminalAggregate, though unlike [[ByNameOps.opaque]]
    * if there is a failure *within* the wrapped parser the failure's location
    * and error message will still be shown
    *
    * Useful for wrapping things like whitespace, code-comment, etc. parsers
    * which can be applied everywhere and are not useful to display to the user
    * as part of the error message.
    */
  def NoTrace[T](p: => P[T])(implicit ctx: P[_]): P[T] = {

    val res = p
    if (ctx.verboseFailures) {
      ctx.failureGroupAggregate = Msgs.empty
      ctx.shortParserMsg = Msgs.empty
    }
    res
  }

  /**
    * No-op parser that always succeeds, consuming zero characters
    */
  def Pass(implicit ctx: P[_]): P[Unit] = {
    val res = ctx.freshSuccessUnit()
    if (ctx.verboseFailures) ctx.setMsg(ctx.index, () => "Pass")
    res
  }

  /**
    * No-op parser that always succeeds with the given value, consuming zero
    * characters
    */
  def Pass[T](v: T)(implicit ctx: P[_]): P[T] = {
    val res = ctx.freshSuccess(v)
    if (ctx.verboseFailures) ctx.setMsg(ctx.index, () => "Pass")
    res
  }

  /**
    * No-op parser that always fails, consuming zero characters
    */
  def Fail(implicit ctx: P[_]): P[Nothing] = {
    val res = ctx.freshFailure()
    if (ctx.verboseFailures) ctx.setMsg(ctx.index, () => "fail")
    res
  }

  /**
    * Parser that always succeeds and returns the current index into the parsed
    * input. Useful for e.g. capturing source locations so when downstream
    * valiation raises errors you can tell the user where in the input the
    * error originated from
    */
  def Index(implicit ctx: P[_]): P[Int] = {
    val res = ctx.freshSuccess(ctx.index)
    if (ctx.verboseFailures) ctx.setMsg(ctx.index, () => "Index")
    res
  }

  /**
    * Parses a single character, any character, as long as there is at least
    * one character for it to parse (i.e. the input isn't at its end)
    */
  def AnyChar(implicit ctx: P[_]): P[Unit] = {
    val startIndex = ctx.index
    val res =
      if (!ctx.input.isReachable(ctx.index)) ctx.freshFailure().asInstanceOf[P[Unit]]
      else ctx.freshSuccessUnit(ctx.index + 1)
    if (ctx.verboseFailures) ctx.aggregateTerminal(startIndex, () => "any-character")
    res
  }

  /**
    * Like [[AnyChar]], but returns the single character it parses. Useful
    * together with [[EagerOps.flatMapX]] to provide one-character-lookahead
    * style parsing: [[SingleChar]] consumes the single character, and then
    * [[EagerOps.flatMapX]] can `match` on that single character and decide
    * which downstream parser you wish to invoke
    */
  def SingleChar(implicit ctx: P[_]): P[Char] = {
    val startIndex = ctx.index
    val res =
      if (!ctx.input.isReachable(ctx.index)) ctx.freshFailure().asInstanceOf[P[Char]]
      else ctx.freshSuccess(ctx.input(ctx.index), ctx.index + 1)
    if (ctx.verboseFailures) ctx.aggregateTerminal(startIndex, () => "any-character")
    res
  }

  /**
    * Parses a single character satisfying the given predicate
    */
  def CharPred(p: Char => Boolean)(implicit ctx: P[_]): P[Unit] = macro MacroImpls.charPredMacro

  /**
    * Parses a single character in one of the input strings representing
    * character classes
    */
  def CharIn(s: String*)(implicit ctx: P[_]): P[Unit] = macro MacroImpls.charInMacro
  /**
    * Parses one or more characters as long as they are contained
    * in one of the input strings representing character classes
    */
  def CharsWhileIn(s: String)
                  (implicit ctx: P[_]): P[Unit] = macro MacroImpls.charsWhileInMacro1

  /**
    * Parses `min` or more characters as long as they are contained
    * in one of the input strings representing character classes
    */
  def CharsWhileIn(s: String, min: Int)
                  (implicit ctx: P[_]): P[Unit] = macro MacroImpls.charsWhileInMacro
  /**
    * Parses one or more characters as long as they satisfy the given
    * predicate
    */
  def CharsWhile(p: Char => Boolean)
                (implicit ctx: P[_]): P[Unit] = macro MacroImpls.charsWhileMacro1

  /**
    * Parses `min` or more characters as long as they satisfy the given
    * predicate
    */
  def CharsWhile(p: Char => Boolean, min: Int)
                (implicit ctx: P[_]): P[Unit] = macro MacroImpls.charsWhileMacro


  /**
    * Allows backtracking regardless of whether cuts happen within the wrapped
    * parser; this is useful for re-using an existing parser with cuts within
    * it, in other parts of your grammar where backtracking is necessary and
    * unavoidable.
    */
  def NoCut[T](parse: => P[T])(implicit ctx: P[_]): P[T] = {
    val cut = ctx.cut
    val oldNoCut = ctx.noDropBuffer
    ctx.noDropBuffer = true
    val res = parse
    ctx.noDropBuffer = oldNoCut

    res.cut = cut
    res
  }


  /**
    * Efficiently parses any one of the given [[String]]s; more efficient than
    * chaining [[EagerOps.|]] together
    */
  def StringIn(s: String*)(implicit ctx: P[_]): P[Unit] = macro MacroImpls.stringInMacro

  /**
    * Efficiently parses any one of the given [[String]]s, case-insensitively;
    * more efficient than chaining [[EagerOps.|]] together with [[IgnoreCase]]
    */
  def StringInIgnoreCase(s: String*)(implicit ctx: P[_]): P[Unit] = macro MacroImpls.stringInIgnoreCaseMacro

}
