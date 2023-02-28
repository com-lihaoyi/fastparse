package fastparse
import fastparse.internal._
import fastparse.internal.{Instrument, Logger}


trait SharedPackageDefs {
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
    * Shorthand for `P[Unit]`
    */
  type P0 = P[Unit]

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


}

object SharedPackageDefs{

  def opaque[T](parse0: () => P[T], msg: String)(implicit ctx: P[Any]): P[T] = {
    val oldIndex = ctx.index

    val res = parse0()

    val res2 =
      if (res.isSuccess) ctx.freshSuccess(ctx.successValue)
      else ctx.freshFailure(oldIndex)

    if (ctx.verboseFailures) ctx.aggregateTerminal(oldIndex, () => msg)

    res2.asInstanceOf[P[T]]
  }

  def unary_!(parse0: () => P[_])(implicit ctx: P[Any]): P[Unit] = {
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


  /** Wraps a parser to log when it succeeds and fails, and at what index.
   * Useful for seeing what is going on within your parser. Nicely indents
   * the logs for easy reading
   */
  def log[T](parse0: () => P[T])(implicit  ctx: P[_], name: sourcecode.Name, logger: Logger = Logger.stdout): P[T] = {
    if (ctx.logDepth == -1) parse0()
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
      parse0()
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

  /** Prints the given message, nicely indented, after the wrapped parser finishes */
  def logAfter[T](parse0: () => P[T], msg: => Any)(implicit  ctx: P[_], logger: Logger = Logger.stdout): P[T] = {
    val indent = "  " * ctx.logDepth
    val res = parse0()
    if (ctx.logDepth != -1) logger.f(indent + msg)
    res
  }

  /** Prints the given message, nicely indented, before the wrapped parser starts */
  def logBefore[T](parse0: () => P[T], msg: => Any)(implicit  ctx: P[_], logger: Logger = Logger.stdout): P[T] = {
    val indent = "  " * ctx.logDepth
    if (ctx.logDepth != -1) logger.f(indent + msg)
    val res = parse0()
    res
  }
}