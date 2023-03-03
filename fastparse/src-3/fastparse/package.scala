import fastparse.internal.{Instrument, Logger, *}

import language.experimental.macros

package object fastparse extends fastparse.SharedPackageDefs {

  /**
   * Shorthand alias for [[ParsingRun]]; this is both the parameter-to and the
   * return type for all Fastparse's parsing methods.
   *
   * @tparam T is the type of the value returned by the parser method on success
   */
  type P[+T] = ParsingRun[T]

  val P = ParsingRun

  implicit def DiscardParserValue(p: P[_]): P[Unit] = {
    p.successValue = ()
    p.asInstanceOf[P[Unit]]
  }

  /** Delimits a named parser. This name will appear in the parser failure
    * messages and stack traces, and by default is taken from the name of the
    * enclosing method.
    */
  inline def P[T](inline t: P[T])(implicit name: sourcecode.Name, ctx: P[_]): P[T] =
    MacroInlineImpls.pInline(t)(name, ctx)

  /** Parses an exact string value. */
  implicit inline def LiteralStr(s: String)(implicit ctx: P[Any]): P[Unit] =
    ${ MacroInlineImpls.literalStrMacro('{ s })('{ ctx }) }

  extension [T](inline parse0: P[T])
    /** Tests the output of the parser with a given predicate, failing the
      * parse if the predicate returns false. Useful for doing local validation
      * on bits and pieces of your parsed output
      */
    inline def filter(f: T => Boolean)(using ctx: P[Any]): P[T] =
      MacroInlineImpls.filterInline[T](parse0)(f)(ctx)

    /** Either-or operator: tries to parse the left-hand-side, and if that
      * fails it backtracks and tries to pass the right-hand-side. Can be
      * chained more than once to parse larger numbers of alternatives.
      */
    inline def |[V >: T](inline other: P[V])(using ctx: P[Any]): P[V] =
      MacroInlineImpls.eitherInline[T, V](parse0)(other)(ctx)

    /** Plain cut operator. Runs the parser, and if it succeeds, backtracking
      * past that point is now prohibited
      */
    inline def /(implicit ctx: P[_]): P[T] = MacroInlineImpls.cutInline[T](parse0)(ctx) // macro MacroImpls.cutMacro[T]

    /** Optional operator. Parses the given input to wrap it in a `Some`, but
      * if parsing fails backtracks and returns `None`
      */
    inline def ?[V](implicit optioner: Implicits.Optioner[T, V], ctx: P[Any]): P[V] =
      MacroInlineImpls.optionInline[T, V](parse0)(optioner, ctx)

    /** Transforms the result of this parser using the given function. Useful
      * for turning the [[String]]s captured by [[!]] and the tuples built
      * by [[~]] into your own case classes or data structure
      */
    inline def map[V](inline f: T => V): P[V] = MacroInlineImpls.mapInline[T, V](parse0)(f)

    /** Transforms the result of this parser using the given partial function,
      * failing the parse if the partial function is not defined on the result
      * of the current parser. This is eqivalent to
      * `.filter(f.isDefinedAt).map(f.apply)`
      */
    inline def collect[V](inline f: PartialFunction[T, V]): P[V] = MacroInlineImpls.collectInline[T, V](parse0)(f)

    /** Transforms the result of this parser using the given function into a
      * new parser which is applied (without consuming whitespace). Useful for
      * doing dependent parsing, e.g. when parsing JSON you may first parse a
      * character to see if it's a `[`, `{`, or `"`, and then deciding whether
      * you next want to parse an array, dictionary or string.
      */
    inline def flatMapX[V](inline f: T => P[V]): P[V] = MacroInlineImpls.flatMapXInline[T, V](parse0)(f)

    /** Transforms the result of this parser using the given function into a
      * new parser which is applied (after whitespace). Useful for doing
      * dependent parsing, e.g. when parsing JSON you may first parse a
      * character to see if it's a `[`, `{`, or `"`, and then deciding whether
      * you next want to parse an array, dictionary or string.
      */
    inline def flatMap[V](f: T => P[V])(using whitespace: Whitespace): P[V] =
      MacroInlineImpls.flatMapInline[T, V](parse0)(f)(whitespace)

    /** Capture operator; makes the parser return the span of input it parsed
      * as a [[String]], which can then be processed further using [[~]],
      * [[map]] or [[flatMapX]]
      */
    inline def !(using ctx: P[Any]): P[String] = MacroInlineImpls.captureInline(parse0)(ctx)

    /** Sequence-with-cut operator. Runs two parsers one after the other,
      * with optional whitespace in between. If the first parser completes
      * successfully, backtracking is now prohibited. If both parsers
      * return a value, this returns a tuple.
      */
    inline def ~/[V, R](inline other: P[V])(using
        s: Implicits.Sequencer[T, V, R],
        whitespace: Whitespace,
        ctx: P[_]
    ): P[R] = ${ MacroInlineImpls.parsedSequence0[T, V, R]('parse0, 'other, true)('s, 'whitespace, 'ctx) }

    /** Sequence operator. Runs two parsers one after the other,
      * with optional whitespace in between.If both parsers
      * return a value, this returns a tuple.
      */
    inline def ~[V, R](inline other: P[V])(using
        s: Implicits.Sequencer[T, V, R],
        whitespace: Whitespace,
        ctx: P[_]
    ): P[R] =
      ${ MacroInlineImpls.parsedSequence0[T, V, R]('parse0, 'other, false)('s, 'whitespace, 'ctx) }

    /** Raw sequence-with-cut operator. Runs two parsers one after the other,
      * *without* whitespace in between. If the first parser completes
      * successfully, backtracking is no longer prohibited. If both parsers
      * return a value, this returns a tuple.
      */
    inline def ~~/[V, R](inline other: P[V])(using s: Implicits.Sequencer[T, V, R], ctx: P[_]): P[R] =
      ${ MacroInlineImpls.parsedSequence0[T, V, R]('parse0, 'other, true)('s, null, 'ctx) }

    /** Raw sequence operator. Runs two parsers one after the other,
      * *without* whitespace in between. If both parsers return a value,
      * this returns a tuple.
      */
    inline def ~~[V, R](inline other: P[V])(using s: Implicits.Sequencer[T, V, R], ctx: P[_]): P[R] =
      ${ MacroInlineImpls.parsedSequence0[T, V, R]('parse0, 'other, false)('s, null, 'ctx) }

    /** Repeat operator; runs the LHS parser 0 or more times separated by the
      * given whitespace (in implicit scope), and returns
      * a `Seq[T]` of the parsed values. On failure, backtracks to the starting
      * index of the last run.
      */
    inline def rep[V](using repeater: Implicits.Repeater[T, V], whitespace: Whitespace, ctx: P[Any]): P[V] =
      ${ MacroRepImpls.repXMacro0[T, V]('parse0, 'whitespace, null)('repeater, 'ctx) }

    /** Raw repeat operator; runs the LHS parser 0 or more times *without*
      * any whitespace in between, and returns
      * a `Seq[T]` of the parsed values. On failure, backtracks to the starting
      * index of the last run.
      */
    inline def repX[V](using repeater: Implicits.Repeater[T, V], ctx: P[Any]): P[V] =
      ${ MacroRepImpls.repXMacro0[T, V]('parse0, null, null)('repeater, 'ctx) }

    /// ** Repeat operator; runs the LHS parser at least `min`
    //  * times separated by the given whitespace (in implicit scope),
    //  * and returns a `Seq[T]` of the parsed values. On
    //  * failure, backtracks to the starting index of the last run.
    //  */
    // inline def rep[V](inline min: Int)(using repeater: Implicits.Repeater[T, V], whitespace: Whitespace, ctx: P[Any]): P[V] =
    //  ${ MacroRepImpls.repXMacro0[T, V]('parse0, 'whitespace, 'min)('repeater, 'ctx) }

    /// ** Raw repeat operator; runs the LHS parser at least `min`
    //  * times *without* any whitespace in between,
    //  * and returns a `Seq[T]` of the parsed values. On
    //  * failure, backtracks to the starting index of the last run.
    //  */
    // inline def repX[V](min: Int)(implicit repeater: Implicits.Repeater[T, V], ctx: P[Any]): P[V] =
    //  ${ MacroRepImpls.repXMacro0[T, V]('parse0, null, 'min)('repeater, 'ctx) }

  end extension

  extension [T](parse0: => P[T])

    /** Repeat operator; runs the LHS parser at least `min` to at most `max`
      * times separated by the given whitespace (in implicit scope) and
      * separator `sep`, and returns a `Seq[T]` of the parsed values. On
      * failure, backtracks to the starting index of the last run.
      *
      * The convenience parameter `exactly` is provided to set both `min` and
      * `max` to the same value.
      */
    inline def rep[V](
        min: Int = 0,
        sep: => P[_] = null,
        inline max: Int = Int.MaxValue,
        inline exactly: Int = -1
    )(using
        repeater: Implicits.Repeater[T, V],
        whitespace: Whitespace,
        ctx: P[Any]
    ): P[V] =
      if max == Int.MaxValue && exactly == -1
      then new RepImpls[T](() => parse0).rep[V](min, sep)
      else new RepImpls[T](() => parse0).rep[V](min, sep, max, exactly)

    /** Raw repeat operator; runs the LHS parser at least `min` to at most `max`
      * times separated by the
      * separator `sep` *without* any whitespace in between, and returns a `Seq[T]` of the parsed values. On
      * failure, backtracks to the starting index of the last run.
      *
      * The convenience parameter `exactly` is provided to set both `min` and
      * `max` to the same value.
      */
    inline def repX[V](
        min: Int = 0,
        sep: => P[_] = null,
        inline max: Int = Int.MaxValue,
        inline exactly: Int = -1
    )(implicit
        repeater: Implicits.Repeater[T, V],
        ctx: P[Any]
    ): P[V] =
      if max == Int.MaxValue && exactly == -1
      then new RepImpls[T](() => parse0).repX[V](min, sep)
      else new RepImpls[T](() => parse0).repX[V](min, sep, max, exactly)


    /**
     * Hides the internals of the given parser when it fails, such that it
     * only succeeds completely or fails completely, and none of it's internal
     * parsers end up in the failure traces or failure stack to be displayed
     * to the user.
     */
    def opaque(msg: String)(implicit ctx: P[Any]): P[T] = SharedPackageDefs.opaque(() => parse0, msg)

    /**
     * Negative lookahead operator: succeeds if the wrapped parser fails and
     * fails if the wrapped parser succeeds. In all cases, it ends up
     * consuming zero characters.
     */
    def unary_!(implicit ctx: P[Any]): P[Unit] = SharedPackageDefs.unary_!(() => parse0)
  end extension

  /** Provides logging-related [[LogByNameOps]] implicits on [[String]]. */
  implicit def LogOpsStr(parse0: String)(implicit ctx: P[Any]): fastparse.LogByNameOps[Unit] = LogByNameOps(parse0)
  // ??? // macro MacroImpls.logOpsStrMacro
  /** Separated out from [[ByNameOps]] because `.log` isn't easy to make an
    * [[AnyVal]] extension method, but it doesn't matter since `.log` calls
    * are only for use in development while the other [[ByNameOps]] operators
    * are more performance-sensitive
    */
  implicit class LogByNameOps[T](parse0: => P[T])(implicit ctx: P[_]) {

    /** Wraps a parser to log when it succeeds and fails, and at what index.
      * Useful for seeing what is going on within your parser. Nicely indents
      * the logs for easy reading
      */
    def log(implicit name: sourcecode.Name, logger: Logger = Logger.stdout): P[T] = SharedPackageDefs.log(() => parse0)

    /** Prints the given message, nicely indented, after the wrapped parser finishes */
    def logAfter(msg: => Any)(implicit logger: Logger = Logger.stdout): P[T] = SharedPackageDefs.logAfter(() => parse0, msg)

    /** Prints the given message, nicely indented, before the wrapped parser starts */
    def logBefore(msg: => Any)(implicit logger: Logger = Logger.stdout): P[T] = SharedPackageDefs.logBefore(() => parse0, msg)
  }


  /** Parses a single character satisfying the given predicate */
  inline def CharPred(inline p: Char => Boolean)(implicit ctx: P[_]): P[Unit] = MacroInlineImpls.charPredInline(p)(ctx)

  /** Parses a single character in one of the input strings representing
    * character classes
    */
  inline def CharIn(inline s: String*)(using ctx: P[_]): P[Unit] =
    ${ MacroInlineImpls.charInMacro('s)('ctx) }

  /** Parses `min` or more characters as long as they are contained
    * in one of the input strings representing character classes
    */
  inline def CharsWhileIn(inline s: String, min: Int = 1)(implicit ctx: P[_]): P[Unit] =
    ${ MacroInlineImpls.charsWhileInMacro('s, 'min)('ctx) }

  /** Parses `min` or more characters as long as they satisfy the given
    * predicate
    */
  inline def CharsWhile(inline p: Char => Boolean, min: Int = 1)(implicit ctx: P[_]): P[Unit] =
    MacroInlineImpls.charsWhileInline(p, min)(ctx)

  /** Efficiently parses any one of the given [[String]]s; more efficient than
    * chaining [[EagerOps.|]] together
    */
  inline def StringIn(inline s: String*)(implicit ctx: P[_]): P[Unit] =
    ${ MacroInlineImpls.stringInMacro0('false, 's)('ctx) }

  /** Efficiently parses any one of the given [[String]]s, case-insensitively;
    * more efficient than chaining [[EagerOps.|]] together with [[IgnoreCase]]
    */
  inline def StringInIgnoreCase(inline s: String*)(implicit ctx: P[_]): P[Unit] =
    ${ MacroInlineImpls.stringInMacro0('true, 's)('ctx) }

}
