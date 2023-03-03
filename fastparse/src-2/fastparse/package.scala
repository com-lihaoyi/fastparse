import fastparse.internal._
import fastparse.internal.{Instrument, Logger}

import language.experimental.macros

package object fastparse extends fastparse.SharedPackageDefs {
  /**
    * Delimits a named parser. This name will appear in the parser failure
    * messages and stack traces, and by default is taken from the name of the
    * enclosing method.
    */
  def P[T](t: P[T])(implicit name: sourcecode.Name, ctx: P[_]): P[T] = macro MacroImpls.pMacro[T]

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

  /**
    * Parses an exact string value.
    */
  implicit def LiteralStr(s: String)(implicit ctx: P[Any]): P[Unit] = macro MacroImpls.literalStrMacro

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
                 whitespace: Whitespace,
                 ctx: P[_]): P[R] = macro MacroImpls.parsedSequenceCut[T, V, R]

    /**
      * Sequence operator. Runs two parsers one after the other,
      * with optional whitespace in between.If both parsers
      * return a value, this returns a tuple.
      */
    def ~[V, R](other:  P[V])
               (implicit s: Implicits.Sequencer[T, V, R],
                whitespace: Whitespace,
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
      * Transforms the result of this parser using the given partial function,
      * failing the parse if the partial function is not defined on the result
      * of the current parser. This is eqivalent to
      * `.filter(f.isDefinedAt).map(f.apply)`
      */
    def collect[V](f: PartialFunction[T, V]): P[V] = macro MacroImpls.collectMacro[T, V]

    /**
      * Transforms the result of this parser using the given function into a
      * new parser which is applied (after whitespace). Useful for doing
      * dependent parsing, e.g. when parsing JSON you may first parse a
      * character to see if it's a `[`, `{`, or `"`, and then deciding whether
      * you next want to parse an array, dictionary or string.
      */
    def flatMap[V](f: T => P[V])
                  (implicit whitespace: Whitespace): P[V] = macro MacroImpls.flatMapMacro[T, V]
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
  implicit def ByNameOps[T](parse0: => P[T]): ByNameOps[T] = new ByNameOps(() => parse0)
  class ByNameOps[T](val parse0: () => P[T]) extends AnyVal{
    /**
      * Repeat operator; runs the LHS parser 0 or more times separated by the
      * given whitespace (in implicit scope), and returns
      * a `Seq[T]` of the parsed values. On failure, backtracks to the starting
      * index of the last run.
      */
    def rep[V](implicit repeater: Implicits.Repeater[T, V],
               whitespace: Whitespace,
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
               whitespace: Whitespace,
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
               whitespace: Whitespace,
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
               whitespace: Whitespace,
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
    def opaque(msg: String)(implicit ctx: P[Any]): P[T] = SharedPackageDefs.opaque(parse0, msg)

    /**
      * Negative lookahead operator: succeeds if the wrapped parser fails and
      * fails if the wrapped parser succeeds. In all cases, it ends up
      * consuming zero characters.
      */
    def unary_!(implicit ctx: P[Any]) : P[Unit] =  SharedPackageDefs.unary_!(parse0)
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
