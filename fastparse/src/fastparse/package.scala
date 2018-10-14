import scala.annotation.tailrec
import scala.reflect.macros.blackbox.Context
import language.experimental.macros
package object fastparse {

  type P[+T] = ParsingRun[T]
  type P0 = ParsingRun[Unit]
  val P = ParsingRun
  /**
    * Delimits a named parser. This name will appear in the parser failure
    * messages and stack traces, and by default is taken from the name of the
    * enclosing method.
    */
  def P[T](t: ParsingRun[T])(implicit name: sourcecode.Name, ctx: ParsingRun[_]): ParsingRun[T] = macro MacroImpls.pMacro[T]


  implicit def LiteralStr(s: String)(implicit ctx: ParsingRun[Any]): ParsingRun[Unit] = macro MacroImpls.literalStrMacro


  def startsWithIgnoreCase(src: ParserInput, prefix: IndexedSeq[Char], offset: Int) = {
    @tailrec def rec(i: Int): Boolean = {
      if (i >= prefix.length) true
      else if(!src.isReachable(i + offset)) false
      else {
        val c1: Char = src(i + offset)
        val c2: Char = prefix(i)
        if (c1 != c2 && c1.toLower != c2.toLower) false
        else rec(i + 1)
      }
    }
    rec(0)
  }
  def IgnoreCase(s: String)(implicit ctx: ParsingRun[Any]): ParsingRun[Unit] = {
    if (startsWithIgnoreCase(ctx.input, s, ctx.index)) ctx.freshSuccess((), Util.literalize(s), ctx.index + s.length)
    else ctx.freshFailure(Util.literalize(s)).asInstanceOf[ParsingRun[Unit]]
  }


  implicit def EagerOpsStr(parse0: String)(implicit ctx: ParsingRun[Any]): fastparse.EagerOps[Unit] = macro MacroImpls.eagerOpsStrMacro

  def parsedSequence[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
  (c: Context)
  (other: c.Expr[ParsingRun[V]])
  (s: c.Expr[Implicits.Sequencer[T, V, R]],
   whitespace: c.Expr[ParsingRun[Any] => ParsingRun[Unit]],
   ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    import c.universe._
    MacroImpls.parsedSequence0[T, V, R](c)(other, false)(s, Some(whitespace), ctx)
  }

  def parsedSequenceCut[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
  (c: Context)
  (other: c.Expr[ParsingRun[V]])
  (s: c.Expr[Implicits.Sequencer[T, V, R]],
   whitespace: c.Expr[ParsingRun[Any] => ParsingRun[Unit]],
   ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    import c.universe._
    MacroImpls.parsedSequence0[T, V, R](c)(other, true)(s, Some(whitespace), ctx)
  }
  def parsedSequence1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
  (c: Context)
  (other: c.Expr[ParsingRun[V]])
  (s: c.Expr[Implicits.Sequencer[T, V, R]],
   ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    import c.universe._
    MacroImpls.parsedSequence0[T, V, R](c)(other, false)(s, None, ctx)
  }
  def parsedSequenceCut1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
  (c: Context)
  (other: c.Expr[ParsingRun[V]])
  (s: c.Expr[Implicits.Sequencer[T, V, R]],
   ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    import c.universe._
    MacroImpls.parsedSequence0[T, V, R](c)(other, true)(s, None, ctx)
  }


  implicit class EagerOps[T](val parse0: ParsingRun[T]) extends AnyVal{

    def ~/[V, R](other: ParsingRun[V])
                (implicit s: Implicits.Sequencer[T, V, R],
                 whitespace: ParsingRun[Any] => ParsingRun[Unit],
                 ctx: ParsingRun[_]): ParsingRun[R] = macro parsedSequenceCut[T, V, R]

    def /(implicit ctx: ParsingRun[_]): ParsingRun[T] = macro MacroImpls.cutMacro[T]

    def ~[V, R](other:  ParsingRun[V])
               (implicit s: Implicits.Sequencer[T, V, R],
                whitespace: ParsingRun[Any] => ParsingRun[Unit],
                ctx: ParsingRun[_]): ParsingRun[R] = macro parsedSequence[T, V, R]


    def ~~/[V, R](other: ParsingRun[V])
                 (implicit s: Implicits.Sequencer[T, V, R],
                  ctx: ParsingRun[_]): ParsingRun[R] = macro parsedSequenceCut1[T, V, R]


    def ~~[V, R](other: ParsingRun[V])
                (implicit s: Implicits.Sequencer[T, V, R],
                 ctx: ParsingRun[_]): ParsingRun[R] = macro parsedSequence1[T, V, R]

    def map[V](f: T => V): ParsingRun[V] = macro MacroImpls.mapMacro[T, V]

    def filter(f: T => Boolean)(implicit ctx: ParsingRun[Any]): ParsingRun[T] = macro MacroImpls.filterMacro[T]

    def flatMap[V](f: T => ParsingRun[V]): ParsingRun[V] = macro MacroImpls.flatMapMacro[T, V]

    def |[V >: T](other: ParsingRun[V])(implicit ctx: ParsingRun[Any]): ParsingRun[V] = macro MacroImpls.eitherMacro[T, V]

    def !(implicit ctx: ParsingRun[Any]): ParsingRun[String] = macro MacroImpls.captureMacro
  }



  implicit def ByNameOpsStr(parse0: String)(implicit ctx: ParsingRun[Any]): fastparse.ByNameOps[Unit] =
  macro MacroImpls.byNameOpsStrMacro

  implicit def ByNameOps[T](parse0: => ParsingRun[T]) = new ByNameOps(() => parse0)
  class ByNameOps[T](val parse0: () => ParsingRun[T]) extends AnyVal{

    def repX[V](implicit repeater: Implicits.Repeater[T, V], ctx: ParsingRun[Any]): ParsingRun[V] =
    macro RepImpls.repXMacro1[T, V]
    def repX[V](min: Int = 0,
                sep: => ParsingRun[_] = null,
                max: Int = Int.MaxValue,
                exactly: Int = -1)
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: ParsingRun[Any]): ParsingRun[V] =
      new RepImpls[T](parse0).repX[V](min, sep, max, exactly)
    def repX[V](min: Int,
                sep: => ParsingRun[_])
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: ParsingRun[Any]): ParsingRun[V] =
      new RepImpls[T](parse0).repX[V](min, sep)
    def repX[V](min: Int)
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: ParsingRun[Any]): ParsingRun[V] =
    macro RepImpls.repXMacro2[T, V]

    def rep[V](implicit repeater: Implicits.Repeater[T, V],
               whitespace: ParsingRun[_] => ParsingRun[Unit],
               ctx: ParsingRun[Any]): ParsingRun[V] =
    macro RepImpls.repXMacro1ws[T, V]
    def rep[V](min: Int = 0,
               sep: => ParsingRun[_] = null,
               max: Int = Int.MaxValue,
               exactly: Int = -1)
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: ParsingRun[_] => ParsingRun[Unit],
               ctx: ParsingRun[Any]): ParsingRun[V] =
      new RepImpls[T](parse0).rep[V](min, sep, max, exactly)
    def rep[V](min: Int,
               sep: => ParsingRun[_])
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: ParsingRun[_] => ParsingRun[Unit],
               ctx: ParsingRun[Any]): ParsingRun[V] =
      new RepImpls[T](parse0).rep[V](min, sep)

    def rep[V](min: Int)
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: ParsingRun[_] => ParsingRun[Unit],
               ctx: ParsingRun[Any]): ParsingRun[V] =
    macro RepImpls.repXMacro2ws[T, V]

    def opaque(msg: String)(implicit ctx: ParsingRun[Any]) = {
      val oldFailures = ctx.failureAggregate
      val oldIndex = ctx.index
      val res = parse0()
      if (ctx.traceIndex != -1){
        ctx.failureAggregate = oldFailures
        if (ctx.traceIndex == oldIndex && !res.isSuccess) {
          ctx.failureStack = Nil
          ctx.aggregateFailure(msg)
        }
      } else if (!res.isSuccess){
        ctx.failureStack = Nil
        ctx.shortFailureMsg = () => msg
        ctx.index = oldIndex
      }
      res
    }


    def unary_!(implicit ctx: ParsingRun[Any]) : ParsingRun[Unit] = {
      val startPos = ctx.index
      val startCut = ctx.cut
      val startFailures = ctx.failureAggregate
      val oldNoCut = ctx.noDropBuffer
      ctx.noDropBuffer = true
      parse0()
      ctx.noDropBuffer = oldNoCut
      val res =
        if (!ctx.isSuccess) ctx.freshSuccess((), null, startPos)
        else {
          val msg = ctx.shortFailureMsg
          val res = ctx.prepareFailure("!" + msg(), startPos)
          // Do not aggregate failures inside the !(...) expression,
          // since those failures are desired to make the parse succeed!
          ctx.failureAggregate = startFailures
          res
        }
      res.cut = startCut
      res
    }

    def ?[V](implicit optioner: Implicits.Optioner[T, V], ctx: ParsingRun[Any]): ParsingRun[V] = {
      val startPos = ctx.index
      val startCut = ctx.cut
      ctx.cut = false
      parse0()
      if (ctx.isSuccess) {
        val msg = ctx.shortFailureMsg
        val res = ctx.prepareSuccess(
          optioner.some(ctx.successValue.asInstanceOf[T]),
          msg() + ".?"
        )
        res.cut |= startCut
        res
      }
      else if (ctx.cut) ctx.asInstanceOf[ParsingRun[V]]
      else {
        val res = ctx.freshSuccess(optioner.none, null, startPos)
        res.cut |= startCut
        res
      }
    }
  }

  implicit def LogOpsStr(parse0: String)(implicit ctx: ParsingRun[Any]): fastparse.LogByNameOps[Unit] =
  macro MacroImpls.logOpsStrMacro
  /**
    * Separated out from [[ByNameOps]] because `.log` isn't easy to make an
    * [[AnyVal]] extension method, but it doesn't matter since `.log` calls
    * are only for use in development while the other [[ByNameOps]] operators
    * are more performance-sensitive
    */
  implicit class  LogByNameOps[T](parse0: => ParsingRun[T])(implicit ctx: ParsingRun[_]) {
    def log(implicit name: sourcecode.Name, logger: Logger = Logger.stdout): ParsingRun[T] = {

      val msg = name.value
      val output = logger.f
      val indent = "  " * ctx.logDepth

      output(s"$indent+$msg:${ctx.input.prettyIndex(ctx.index)}${if (ctx.cut) ", cut" else ""}")
      val depth = ctx.logDepth
      ctx.logDepth += 1
      val startIndex = ctx.index
      parse0
      ctx.logDepth = depth
      val strRes = if (ctx.isSuccess){
        val prettyIndex = ctx.input.prettyIndex(ctx.index)
        s"Success($prettyIndex${if (ctx.cut) ", cut" else ""})"
      } else{
        val trace = Parsed.Failure.formatStack(
          ctx.input,
          (Option(ctx.shortFailureMsg).fold("")(_()) -> ctx.index) :: ctx.failureStack.reverse
        )
        val trailing = ctx.input match{
          case c: IndexedParserInput => Parsed.Failure.formatTrailing(ctx.input, startIndex)
          case _ => ""
        }
        s"Failure($trace ...$trailing${if (ctx.cut) ", cut" else ""})"
      }
      output(s"$indent-$msg:${ctx.input.prettyIndex(startIndex)}:$strRes")
      //        output(s"$indent-$msg:${repr.prettyIndex(cfg.input, index)}:$strRes")
      ctx.asInstanceOf[ParsingRun[T]]
    }

  }

  def &(parse: => ParsingRun[_])(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = {

    val startPos = ctx.index
    val startCut = ctx.cut
    val oldNoCut = ctx.noDropBuffer
    ctx.noDropBuffer = true
    parse
    ctx.noDropBuffer = oldNoCut
    val msg = ctx.shortFailureMsg
    val res =
      if (ctx.isSuccess) ctx.prepareSuccess((), s"&(${msg()})", startPos)
      else ctx.asInstanceOf[ParsingRun[Unit]]
    res.cut = startCut
    res

  }

  def End(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = {
    if (!ctx.input.isReachable(ctx.index)) ctx.freshSuccess((), "end-of-input")
    else ctx.freshFailure("end-of-input").asInstanceOf[ParsingRun[Unit]]

  }

  def Start(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = {
    if (ctx.index == 0) ctx.freshSuccess((), "start-of-input")
    else ctx.freshFailure("start-of-input").asInstanceOf[ParsingRun[Unit]]

  }

  def Pass(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = ctx.freshSuccess((), "")
  def NoTrace[T](p: => ParsingRun[T])(implicit ctx: ParsingRun[_]): ParsingRun[T] = {
    val preMsg = ctx.failureAggregate
    val res = p
    if (ctx.traceIndex != -1) ctx.failureAggregate = preMsg
    res
  }
  def Pass[T](v: T)(implicit ctx: ParsingRun[_]): ParsingRun[T] = ctx.freshSuccess(v, "")

  def Fail(implicit ctx: ParsingRun[_]): ParsingRun[Nothing] = ctx.freshFailure("failure").asInstanceOf[ParsingRun[Nothing]]

  def Index(implicit ctx: ParsingRun[_]): ParsingRun[Int] = ctx.freshSuccess(ctx.index, "")

  def AnyChar(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = {
    if (!ctx.input.isReachable(ctx.index)) ctx.freshFailure("any-character").asInstanceOf[ParsingRun[Unit]]
    else ctx.freshSuccess((), "any-character", ctx.index + 1)
  }
  def SingleChar(implicit ctx: ParsingRun[_]): ParsingRun[Char] = {
    if (!ctx.input.isReachable(ctx.index)) ctx.freshFailure("any-character").asInstanceOf[ParsingRun[Char]]
    else ctx.freshSuccess(ctx.input(ctx.index), "any-character", ctx.index + 1)
  }
  def CharPred(p: Char => Boolean)(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = {
    if (!(ctx.input.isReachable(ctx.index) && p(ctx.input(ctx.index)))) ctx.freshFailure("character-predicate").asInstanceOf[ParsingRun[Unit]]
    else ctx.freshSuccess((), "character-predicate", ctx.index + 1)
  }
  def CharIn(s: String*)(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = macro MacroImpls.charInMacro

  def CharsWhileIn(s: String)
                  (implicit ctx: ParsingRun[_]): ParsingRun[Unit] = macro MacroImpls.charsWhileInMacro1
  def CharsWhileIn(s: String, min: Int)
                  (implicit ctx: ParsingRun[_]): ParsingRun[Unit] = macro MacroImpls.charsWhileInMacro

  def CharsWhile(p: Char => Boolean, min: Int = 1)(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = {
    var index = ctx.index
    val input = ctx.input


    val start = index
    while(input.isReachable(index) && p(input(index))) index += 1
    if (index - start >= min) ctx.freshSuccess((), s"chars-while($min)", index = index)
    else {
      if (ctx.index == ctx.traceIndex) ctx.aggregateFailure(s"chars-while($min)")
      ctx.isSuccess = false
      ctx.shortFailureMsg = () => s"chars-while($min)"
      ctx.asInstanceOf[ParsingRun[Unit]]
    }
  }

  def NoCut[T](parse: => ParsingRun[T])(implicit ctx: ParsingRun[_]): ParsingRun[T] = {
    val cut = ctx.cut
    val oldNoCut = ctx.noDropBuffer
    ctx.noDropBuffer = true
    val res = parse
    ctx.noDropBuffer = oldNoCut

    res.cut = cut
    res
  }


  def StringIn(s: String*)(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = macro MacroImpls.stringInMacro
  def StringInIgnoreCase(s: String*)(implicit ctx: ParsingRun[_]): ParsingRun[Unit] = macro MacroImpls.stringInIgnoreCaseMacro

  def parseInput[T](input: ParserInput,
                    parser: ParsingRun[_] => ParsingRun[T],
                    startIndex: Int = 0,
                    traceIndex: Int = -1,
                    instrument: ParsingRun.Instrument = null): Parsed[T] = parser(new ParsingRun(
    input = input,
    shortFailureMsg = null,
    failureStack = List.empty,
    failureAggregate = List.empty,
    isSuccess = true,
    logDepth = 0,
    startIndex, startIndex, true, (), traceIndex, parser, false, instrument
  )).result
  def parseIterator[T](input: Iterator[String],
                       parser: ParsingRun[_] => ParsingRun[T],
                       startIndex: Int = 0,
                       traceIndex: Int = -1,
                       instrument: ParsingRun.Instrument = null): Parsed[T] = parseInput(
    input = IteratorParserInput(input),
    parser = parser,
    startIndex = startIndex,
    traceIndex = traceIndex,
    instrument = instrument
  )
  def parse[T](input: String,
               parser: ParsingRun[_] => ParsingRun[T],
               startIndex: Int = 0,
               traceIndex: Int = -1,
               instrument: ParsingRun.Instrument = null): Parsed[T] = parseInput(
    input = IndexedParserInput(input),
    parser = parser,
    startIndex = startIndex,
    traceIndex = traceIndex,
    instrument = instrument
  )
}
