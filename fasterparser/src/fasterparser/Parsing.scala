package fasterparser

import scala.annotation.{switch, tailrec}
import language.experimental.macros
import reflect.macros.blackbox.Context
case class Logger(f: String => Unit)
object Logger {
  implicit val stdout = Logger(println)
}
object Parsing {

  type P[+T] = Parse[T]
  type P0 = Parse[Unit]

  def P[T](t: Parse[T])(implicit name: sourcecode.Name): Parse[T] = macro MacroImpls.pMacro[T]


  implicit def LiteralStr(s: String)(implicit ctx: Parse[Any]): Parse[Unit] = macro MacroImpls.literalStrMacro


  def startsWithIgnoreCase(src: String, prefix: IndexedSeq[Char], offset: Int) = {
    @tailrec def rec(i: Int): Boolean = {
      if (i >= prefix.length) true
      else if(i + offset >= src.length) false
      else {
        val c1: Char = src(i + offset)
        val c2: Char = prefix(i)
        if (c1 != c2 && c1.toLower != c2.toLower) false
        else rec(i + 1)
      }
    }
    rec(0)
  }
  def IgnoreCase(s: String)(implicit ctx: Parse[Any]): Parse[Unit] = {

    if (startsWithIgnoreCase(ctx.input, s, ctx.index)) ctx.freshSuccess((), Util.literalize(s), ctx.index + s.length)
    else ctx.freshFailure(Util.literalize(s)).asInstanceOf[Parse[Unit]]
  }


  implicit def EagerOpsStr(parse0: String)(implicit ctx: Parse[Any]): fasterparser.Parsing.EagerOps[Unit] = macro MacroImpls.eagerOpsStrMacro

  def parsedSequence[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parse[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]],
                     whitespace: c.Expr[Parse[Any] => Parse[Unit]]): c.Expr[Parse[R]] = {
    import c.universe._
    MacroImpls.parsedSequence0[T, V, R](c)(other, false)(s, Some(whitespace))
  }

  def parsedSequenceCut[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parse[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]],
                     whitespace: c.Expr[Parse[Any] => Parse[Unit]]): c.Expr[Parse[R]] = {
    import c.universe._
    MacroImpls.parsedSequence0[T, V, R](c)(other, true)(s, Some(whitespace))
  }
  def parsedSequence1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parse[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]]): c.Expr[Parse[R]] = {
    import c.universe._
    MacroImpls.parsedSequence0[T, V, R](c)(other, false)(s, None)
  }
  def parsedSequenceCut1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parse[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]]): c.Expr[Parse[R]] = {
    import c.universe._
    MacroImpls.parsedSequence0[T, V, R](c)(other, true)(s, None)
  }


  implicit class EagerOps[T](val parse0: Parse[T]) extends AnyVal{

    def ~/[V, R](other: Parse[V])
                (implicit s: Implicits.Sequencer[T, V, R],
                 whitespace: Parse[Any] => Parse[Unit]): Parse[R] = macro parsedSequenceCut[T, V, R]

    def / : Parse[T] = macro MacroImpls.cutMacro[T]

    def ~[V, R](other:  Parse[V])
               (implicit s: Implicits.Sequencer[T, V, R],
                whitespace: Parse[Any] => Parse[Unit]): Parse[R] = macro parsedSequence[T, V, R]


    def ~~/[V, R](other: Parse[V])
                  (implicit s: Implicits.Sequencer[T, V, R]): Parse[R] = macro parsedSequenceCut1[T, V, R]


    def ~~[V, R](other: Parse[V])
                (implicit s: Implicits.Sequencer[T, V, R]): Parse[R] = macro parsedSequence1[T, V, R]

    def map[V](f: T => V): Parse[V] = macro MacroImpls.mapMacro[T, V]

    def flatMap[V](f: T => Parse[V]): Parse[V] = macro MacroImpls.flatMapMacro[T, V]

    def |[V >: T](other: Parse[V])(implicit ctx: Parse[Any]): Parse[V] = macro MacroImpls.eitherMacro[T, V]

    def !(implicit ctx: Parse[Any]): Parse[String] = macro MacroImpls.captureMacro
  }



  implicit def ByNameOpsStr(parse0: String)(implicit ctx: Parse[Any]): fasterparser.Parsing.ByNameOps[Unit] =
    macro MacroImpls.byNameOpsStrMacro

  implicit def ByNameOps[T](parse0: => Parse[T]) = new ByNameOps(() => parse0)
  class ByNameOps[T](val parse0: () => Parse[T]) extends AnyVal{

    def repX[V](implicit repeater: Implicits.Repeater[T, V], ctx: Parse[Any]): Parse[V] =
      macro RepImpls.repXMacro1[T, V]
    def repX[V](min: Int = 0,
                sep: => Parse[_] = null,
                max: Int = Int.MaxValue,
                exactly: Int = -1)
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: Parse[Any]): Parse[V] =
      new RepImpls[T](parse0).repX[V](min, sep, max, exactly)
    def repX[V](min: Int,
                sep: => Parse[_])
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: Parse[Any]): Parse[V] =
        new RepImpls[T](parse0).repX[V](min, sep)
    def repX[V](min: Int)
               (implicit repeater: Implicits.Repeater[T, V],
                ctx: Parse[Any]): Parse[V] =
    macro RepImpls.repXMacro2[T, V]

    def rep[V](implicit repeater: Implicits.Repeater[T, V],
               whitespace: Parse[_] => Parse[Unit],
               ctx: Parse[Any]): Parse[V] =
      macro RepImpls.repXMacro1ws[T, V]
    def rep[V](min: Int = 0,
               sep: => Parse[_] = null,
               max: Int = Int.MaxValue,
               exactly: Int = -1)
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: Parse[_] => Parse[Unit],
               ctx: Parse[Any]): Parse[V] =
      new RepImpls[T](parse0).rep[V](min, sep, max, exactly)
    def rep[V](min: Int,
             sep: => Parse[_])
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: Parse[_] => Parse[Unit],
             ctx: Parse[Any]): Parse[V] =
      new RepImpls[T](parse0).rep[V](min, sep)

    def rep[V](min: Int)
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: Parse[_] => Parse[Unit],
             ctx: Parse[Any]): Parse[V] =
    macro RepImpls.repXMacro2ws[T, V]

    def opaque(msg: String)(implicit ctx: Parse[Any]) = {
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


    def unary_!(implicit ctx: Parse[Any]) : Parse[Unit] = {
      val startPos = ctx.index
      val startCut = ctx.cut
      val startFailures = ctx.failureAggregate
      parse0()
      val res =
        if (!ctx.isSuccess) ctx.freshSuccess((), null, startPos)
        else {
          val res = ctx.prepareFailure(startPos)
          // Do not aggregate failures inside the !(...) expression,
          // since those failures are desired to make the parse succeed!
          ctx.failureAggregate = startFailures
          res
        }
      res.cut = startCut
      res
    }

    def ?[V](implicit optioner: Implicits.Optioner[T, V], ctx: Parse[Any]): Parse[V] = {
      val startPos = ctx.index
      val startCut = ctx.cut
      ctx.cut = false
      parse0()
      if (ctx.isSuccess) {
        val res = ctx.prepareSuccess(optioner.some(ctx.successValue.asInstanceOf[T]))
        res.cut = startCut
        res
      }
      else if (ctx.cut) ctx.asInstanceOf[Parse[V]]
      else {
        val res = ctx.freshSuccess(optioner.none, null, startPos)
        res.cut = startCut
        res
      }
    }

    def filter(f: T => Boolean)(implicit ctx: Parse[Any]): Parse[T] = {
      parse0()
      if (!ctx.isSuccess) ctx.asInstanceOf[Parse[T]]
      else if (f(ctx.successValue.asInstanceOf[T])) ctx.asInstanceOf[Parse[T]]
      else {
        val prevCut = ctx.cut
        val res = ctx.freshFailure("filter").asInstanceOf[Parse[T]]
        res.cut = prevCut
        res
      }
    }
  }
  implicit def LogByNameOps[T](parse0: => Parse[T])(implicit ctx: Parse[_]) = {
    new LogByNameOps(() => parse0)(ctx)
  }

  class LogByNameOps[T](parse0: () => Parse[T])(ctx: Parse[_]) {
    def log(implicit name: sourcecode.Name, logger: Logger = Logger.stdout): Parse[T] = {

      val msg = name.value
      val output = logger.f
      val indent = "  " * ctx.logDepth

      output(s"$indent+$msg:${Util.prettyIndex(ctx.input, ctx.index)}${if (ctx.cut) ", cut" else ""}")
      val depth = ctx.logDepth
      ctx.logDepth += 1
      val startIndex = ctx.index
      parse0()
      ctx.logDepth = depth
      val strRes = if (ctx.isSuccess){
        val prettyIndex = Util.prettyIndex(ctx.input, ctx.index)
        s"Success($prettyIndex${if (ctx.cut) ", cut" else ""})"
      } else{
        val trace = Result.Failure.formatStack(
          ctx.input,
          (Option(ctx.shortFailureMsg).fold("")(_()) -> ctx.index) :: ctx.failureStack.reverse
        )
        val trailing = Result.Failure.formatTrailing(ctx.input, startIndex)
        s"Failure($trace ...$trailing${if (ctx.cut) ", cut" else ""})"
      }
      output(s"$indent-$msg:${Util.prettyIndex(ctx.input, startIndex)}:$strRes")
      //        output(s"$indent-$msg:${repr.prettyIndex(cfg.input, index)}:$strRes")
      ctx.asInstanceOf[Parse[T]]
    }

  }

  def &(parse: => Parse[_])(implicit ctx: Parse[_]): Parse[Unit] = {

    val startPos = ctx.index
    val startCut = ctx.cut
    parse
    val res =
      if (ctx.isSuccess) ctx.prepareSuccess((), startPos)
      else ctx.asInstanceOf[Parse[Unit]]
    res.cut = startCut
    res

  }

  def End(implicit ctx: Parse[_]): Parse[Unit] = {
    if (ctx.index == ctx.input.length) ctx.freshSuccess((), "end-of-input")
    else ctx.freshFailure("end-of-input").asInstanceOf[Parse[Unit]]

  }

  def Start(implicit ctx: Parse[_]): Parse[Unit] = {
    if (ctx.index == 0) ctx.freshSuccess((), "start-of-input")
    else ctx.freshFailure("start-of-input").asInstanceOf[Parse[Unit]]

  }

  def Pass(implicit ctx: Parse[_]): Parse[Unit] = ctx.freshSuccess((), null)
  def NoTrace[T](p: => Parse[T])(implicit ctx: Parse[_]): Parse[T] = {
    val preMsg = ctx.failureAggregate
    val res = p
    if (ctx.traceIndex != -1) ctx.failureAggregate = preMsg
    res
  }
  def Pass[T](v: T)(implicit ctx: Parse[_]): Parse[T] = ctx.freshSuccess(v, null)

  def Fail(implicit ctx: Parse[_]): Parse[Nothing] = ctx.freshFailure("failure").asInstanceOf[Parse[Nothing]]

  def Index(implicit ctx: Parse[_]): Parse[Int] = ctx.freshSuccess(ctx.index, null)

  def AnyChar(implicit ctx: Parse[_]): Parse[Unit] = {
    if (!(ctx.index < ctx.input.length)) ctx.freshFailure("any-character").asInstanceOf[Parse[Unit]]
    else ctx.freshSuccess((), "any-character", ctx.index + 1)
  }
  def SingleChar(implicit ctx: Parse[_]): Parse[Char] = {
    if (!(ctx.index < ctx.input.length)) ctx.freshFailure("any-character").asInstanceOf[Parse[Char]]
    else ctx.freshSuccess(ctx.input(ctx.index), "any-character", ctx.index + 1)
  }
  def CharPred(p: Char => Boolean)(implicit ctx: Parse[_]): Parse[Unit] = {
    if (!(ctx.index < ctx.input.length && p(ctx.input(ctx.index)))) ctx.freshFailure("character-predicate").asInstanceOf[Parse[Unit]]
    else ctx.freshSuccess((), "character-predicate", ctx.index + 1)
  }
  def CharIn(s: String*)(implicit ctx: Parse[_]): Parse[Unit] = macro MacroImpls.charInMacro

  def CharsWhileIn(s: String)
                 (implicit ctx: Parse[_]): Parse[Unit] = macro MacroImpls.charsWhileInMacro1
  def CharsWhileIn(s: String, min: Int)
                 (implicit ctx: Parse[_]): Parse[Unit] = macro MacroImpls.charsWhileInMacro

  def CharsWhile(p: Char => Boolean, min: Int = 1)(implicit ctx: Parse[_]): Parse[Unit] = {
    var index = ctx.index
    val input = ctx.input
    val inputLength = input.length


    val start = index
    while(index < inputLength && p(input(index))) index += 1
    if (index - start >= min) ctx.freshSuccess((), s"chars-while($min)", index = index)
    else {
      if (ctx.index == ctx.traceIndex) ctx.aggregateFailure(s"chars-while($min)")
      ctx.isSuccess = false
      ctx.shortFailureMsg = () => s"chars-while($min)"
      ctx.asInstanceOf[Parse[Unit]]
    }
  }

  def NoCut[T](parse: => Parse[T])(implicit ctx: Parse[_]): Parse[T] = {
    val cut = ctx.cut
    val res = parse
    res.cut = cut
    res
  }


  def StringIn(s: String*)(implicit ctx: Parse[_]): Parse[Unit] = macro MacroImpls.stringInMacro
  def StringInIgnoreCase(s: String*)(implicit ctx: Parse[_]): Parse[Unit] = macro MacroImpls.stringInIgnoreCaseMacro

}

