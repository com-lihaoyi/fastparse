package fasterparser

import scala.annotation.{switch, tailrec}
import language.experimental.macros
import reflect.macros.blackbox.Context

object Parsing {

  type P[+T] = Parse[T]

  def P[T](t: Parse[T])(implicit name: sourcecode.Name): Parse[T] = macro pMacro[T]

  def pMacro[T: c.WeakTypeTag](c: Context)
                              (t: c.Expr[Parse[T]])
                              (name: c.Expr[sourcecode.Name]): c.Expr[Parse[T]] = {

    import c.universe._
    reify[Parse[T]]{
      t.splice// match{case ctx0 =>
//        if (!ctx0.isSuccess) ctx0.failureStack = name.splice.value :: ctx0.failureStack
//        ctx0
//      }
    }
  }

  implicit def LiteralStr(s: String)(implicit ctx: Parse[Any]): Parse[Unit] = {

    if (ctx.input.startsWith(s, ctx.index)) ctx.freshSuccess((), ctx.index + s.length)
    else ctx.freshFailure().asInstanceOf[Parse[Unit]]
  }

  def cutMacro[T: c.WeakTypeTag](c: Context): c.Expr[Parse[T]] = {
    import c.universe._
    val lhs = c.prefix.asInstanceOf[c.Expr[EagerOps[_]]]
    reify{
      val ctx1 = lhs.splice.parse0
      if (ctx1.isSuccess) ctx1.prepareSuccess(ctx1.successValue, cut = true).asInstanceOf[Parse[T]]
      else ctx1.prepareFailure(ctx1.index)
    }
  }

  implicit def EagerOpsStr(parse0: String)(implicit ctx: Parse[Any]): EagerOps[Unit] = {
    EagerOps(LiteralStr(parse0)(ctx))
  }

  def parsedSequence0[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                     (c: Context)
                     (other: c.Expr[Parse[V]], cut: Boolean)
                     (s: c.Expr[Implicits.Sequencer[T, V, R]],
                      whitespace: Option[c.Expr[Parse[Any] => Parse[Unit]]]): c.Expr[Parse[R]] = {
    import c.universe._

    val lhs = c.prefix.asInstanceOf[Expr[EagerOps[T]]]
    val cut1 = c.Expr[Boolean](if(cut) q"true" else q"false")
    val consumeWhitespace = whitespace match{
      case None => reify{(c: Parse[Any]) => true}
      case Some(ws) => reify{(c: Parse[Any]) => ws.splice(c); c.isSuccess}
    }

    reify {
      {
        lhs.splice.parse0 match{ case ctx3 =>
          if (!ctx3.isSuccess) ctx3
          else {
            val pValue = ctx3.successValue
            val pCut = ctx3.successCut

            if (!consumeWhitespace.splice(ctx3)) ctx3
            else {
              other.splice
              if (!ctx3.isSuccess){
                ctx3.prepareFailure(ctx3.index, cut = cut1.splice | ctx3.failureCut | pCut)
              }else {
                ctx3.prepareSuccess(
                  s.splice.apply(pValue.asInstanceOf[T], ctx3.successValue.asInstanceOf[V]),
                  cut = pCut | cut1.splice | ctx3.successCut
                )
              }
            }
          }

        }
      }.asInstanceOf[Parse[R]]
    }
  }

  def parsedSequence[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parse[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]],
                     whitespace: c.Expr[Parse[Any] => Parse[Unit]]): c.Expr[Parse[R]] = {
    import c.universe._
    parsedSequence0[T, V, R](c)(other, false)(s, Some(whitespace))
  }

  def parsedSequenceCut[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parse[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]],
                     whitespace: c.Expr[Parse[Any] => Parse[Unit]]): c.Expr[Parse[R]] = {
    import c.universe._
    parsedSequence0[T, V, R](c)(other, true)(s, Some(whitespace))
  }
  def parsedSequence1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parse[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]]): c.Expr[Parse[R]] = {
    import c.universe._
    parsedSequence0[T, V, R](c)(other, false)(s, None)
  }
  def parsedSequenceCut1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parse[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]]): c.Expr[Parse[R]] = {
    import c.universe._
    parsedSequence0[T, V, R](c)(other, true)(s, None)
  }


  implicit class EagerOps[T](val parse0: Parse[T]) extends AnyVal{

    def ~/[V, R](other: Parse[V])
                (implicit s: Implicits.Sequencer[T, V, R],
                 whitespace: Parse[Any] => Parse[Unit]): Parse[R] = macro parsedSequenceCut[T, V, R]

    def / : Parse[T] = macro cutMacro[T]

    def ~[V, R](other:  Parse[V])
               (implicit s: Implicits.Sequencer[T, V, R],
                whitespace: Parse[Any] => Parse[Unit]): Parse[R] = macro parsedSequence[T, V, R]


    def ~~/[V, R](other: Parse[V])
                  (implicit s: Implicits.Sequencer[T, V, R]): Parse[R] = macro parsedSequenceCut1[T, V, R]


    def ~~[V, R](other: Parse[V])
                (implicit s: Implicits.Sequencer[T, V, R]): Parse[R] = macro parsedSequence1[T, V, R]

    def map[V](f: T => V): Parse[V] = macro mapMacro[T, V]

    def flatMap[V](f: T => Parse[V]): Parse[V] = macro flatMapMacro[T, V]

    def |[V >: T](other: Parse[V])(implicit ctx: Parse[Any]): Parse[V] = macro eitherMacro[T, V]

    def !(implicit ctx: Parse[Any]): Parse[String] = macro captureMacro
  }


  def mapMacro[T: c.WeakTypeTag, V: c.WeakTypeTag]
              (c: Context)
              (f: c.Expr[T => V]): c.Expr[Parse[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      lhs0.splice.parse0 match{ case lhs =>
        if (!lhs.isSuccess) lhs.asInstanceOf[Parse[V]]
        else {
        val this2 = lhs.asInstanceOf[Parse[V]]
        this2.successValue = f.splice(this2.successValue.asInstanceOf[T])
        this2
        }
      }
    }
  }


  def flatMapMacro[T: c.WeakTypeTag, V: c.WeakTypeTag]
                  (c: Context)
                  (f: c.Expr[T => Parse[V]]): c.Expr[Parse[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      val lhs = lhs0.splice.parse0
      if (!lhs.isSuccess) lhs.asInstanceOf[Parse[V]]
      else f.splice(lhs.successValue.asInstanceOf[T])
    }
  }

  def eitherMacro[T: c.WeakTypeTag, V >: T: c.WeakTypeTag]
                 (c: Context)
                 (other: c.Expr[Parse[V]])
                 (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      val ctx5 = ctx.splice.asInstanceOf[Parse[V]]
      val startPos = ctx5.index
      lhs0.splice
      if (ctx5.isSuccess | ctx5.failureCut) ctx5
      else {
        ctx5.index = startPos
        other.splice
        if (ctx5.isSuccess) ctx5
        else if (ctx5.failureCut) ctx5
        else ctx5.prepareFailure(startPos)
      }

    }
  }

  def captureMacro(c: Context)
                  (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[String]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[_]]]

    reify {
      val ctx6 = ctx.splice
      val startPos = ctx6.index
      lhs0.splice
      if (!ctx6.isSuccess) ctx6.asInstanceOf[Parse[String]]
      else ctx6.freshSuccess(ctx6.input.substring(startPos, ctx6.index))
    }
  }

  implicit def ByNameOpsStr[T](parse0: => String)(implicit ctx: Parse[Any]) =
    ByNameOps(LiteralStr(parse0)(ctx))(ctx)

  implicit class ByNameOps[T](parse0: => Parse[T])(implicit val ctx: Parse[Any]){

    def repX[V](implicit repeater: Implicits.Repeater[T, V]): Parse[V] = repX(sep=null)
    def repX[V](min: Int = 0,
                max: Int = Int.MaxValue,
                exactly: Int = -1,
                sep: => Parse[_] = null)(implicit repeater: Implicits.Repeater[T, V]): Parse[V] = {

      val acc = repeater.initial
      val actualMin = if(exactly == -1) min else exactly
      val actualMax = if(exactly == -1) max else exactly
      def end(successIndex: Int, index: Int, count: Int) = {
        if (count < actualMin) ctx.prepareFailure(index)
        else ctx.prepareSuccess(repeater.result(acc), successIndex)
      }
      @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
        if (count == 0 && actualMax == 0) ctx.prepareSuccess(repeater.result(acc), startIndex)
        else {
          parse0
          if (!ctx.isSuccess) {
            if (ctx.failureCut | precut) ctx.asInstanceOf[Parse[V]]
            else end(startIndex, startIndex, count)
          }else {
            val beforeSepIndex = ctx.index
            repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
            if (count + 1 == actualMax) end(beforeSepIndex, beforeSepIndex, count + 1)
            else if (sep == null) rec(beforeSepIndex, count+1, false)
            else {
              if (ctx.isSuccess) rec(beforeSepIndex, count+1, ctx.successCut)
              else if (ctx.failureCut) ctx.prepareFailure(beforeSepIndex)
              else end(beforeSepIndex, beforeSepIndex, count+1)
            }
          }
        }
      }

      val res = rec(ctx.index, 0, false)

      res
    }
    def rep[V](implicit repeater: Implicits.Repeater[T, V],
               whitespace: Parse[_] => Parse[Unit]): Parse[V] = rep(sep=null)
    def rep[V](min: Int = 0,
               max: Int = Int.MaxValue,
               exactly: Int = -1,
               sep: => Parse[_] = null)
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: Parse[_] => Parse[Unit]): Parse[V] = {


      val acc = repeater.initial
      val actualMin = if(exactly == -1) min else exactly
      val actualMax = if(exactly == -1) max else exactly
      def end(successIndex: Int, index: Int, count: Int) = {
        if (count < actualMin) ctx.prepareFailure(index)
        else ctx.prepareSuccess(repeater.result(acc), successIndex)
      }
      @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
        whitespace(ctx)
        if (!ctx.isSuccess) ctx.asInstanceOf[Parse[V]]
        else{
          if (count == 0 && actualMax == 0) ctx.prepareSuccess(repeater.result(acc), startIndex)
          else {
            parse0
            if (!ctx.isSuccess){
              if (ctx.failureCut | precut) ctx.asInstanceOf[Parse[V]]
              else end(startIndex, startIndex, count)
            }else{
              val beforeSepIndex = ctx.index
              repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
              if (count + 1 == actualMax) end(beforeSepIndex, beforeSepIndex, count + 1)
              else if (sep == null) rec(beforeSepIndex, count+1, false)
              else {
                if (ctx.isSuccess) rec(beforeSepIndex, count+1, ctx.successCut)
                else if (ctx.failureCut) ctx.prepareFailure(beforeSepIndex)
                else end(beforeSepIndex, beforeSepIndex, count+1)
              }

            }
          }
        }
      }
      rec(ctx.index, 0, false)
    }

    def literalize(s: IndexedSeq[Char], unicode: Boolean = true) = {
      val sb = new StringBuilder
      sb.append('"')
      var i = 0
      val len = s.length
      while (i < len) {
        (s(i): @switch) match {
          case '"' => sb.append("\\\"")
          case '\\' => sb.append("\\\\")
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case c =>
            if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
            else sb.append(c)
        }
        i += 1
      }
      sb.append('"')

      sb.result()
    }

    def log(implicit name: sourcecode.Name): Parse[T] = {
      if (ctx.logDepth == -1) parse0
      else{
        val msg = name.value
        val output = println(_: String)
        val indent = "  " * ctx.logDepth

        output(s"$indent+$msg:${ctx.index}")
        val depth = ctx.logDepth
        ctx.logDepth += 1
        parse0
        ctx.logDepth = depth
        val (index, strRes) = if (ctx.isSuccess){
          ctx.index -> s"Success(${literalize(ctx.input.slice(ctx.index, ctx.index + 20))}${if (ctx.successCut) ", cut" else ""})"
        } else{
          val trace = ""
          ctx.index -> s"Failure($trace${if (ctx.failureCut) ", cut" else ""})"
        }
        output(s"$indent-$msg:$index:$strRes")
//        output(s"$indent-$msg:${repr.prettyIndex(cfg.input, index)}:$strRes")
        ctx.asInstanceOf[Parse[T]]
      }
    }



    def unary_! : Parse[Unit] = {
      val startPos = ctx.index
      parse0
      if (!ctx.isSuccess) ctx.freshSuccess((), startPos)
      else ctx.prepareFailure(startPos)
    }

    def ?[V](implicit optioner: Implicits.Optioner[T, V]): Parse[V] = {
      val startPos = ctx.index
      parse0
      if (ctx.isSuccess) ctx.prepareSuccess(optioner.some(ctx.successValue.asInstanceOf[T]))
      else if (ctx.failureCut) ctx.asInstanceOf[Parse[V]]
      else ctx.freshSuccess(optioner.none, startPos)
    }


    def filter(f: T => Boolean): Parse[T] = {
      parse0
      if (!ctx.isSuccess) ctx.asInstanceOf[Parse[T]]
      else if (f(ctx.successValue.asInstanceOf[T])) ctx.asInstanceOf[Parse[T]]
      else ctx.freshFailure().asInstanceOf[Parse[T]]
    }
  }

  def &(parse: => Parse[_])(implicit ctx: Parse[_]): Parse[Unit] = {

    val startPos = ctx.index
    parse
    if (ctx.isSuccess) ctx.prepareSuccess((), startPos)
    else ctx.asInstanceOf[Parse[Unit]]

  }

  def End(implicit ctx: Parse[_]): Parse[Unit] = {
    if (ctx.index == ctx.input.length) ctx.freshSuccess(())
    else ctx.freshFailure().asInstanceOf[Parse[Unit]]
  }

  def Start(implicit ctx: Parse[_]): Parse[Unit] = {
    if (ctx.index == 0) ctx.freshSuccess(())
    else ctx.freshFailure().asInstanceOf[Parse[Unit]]

  }

  def Pass(implicit ctx: Parse[_]): Parse[Unit] = ctx.freshSuccess(())
  def Pass[T](v: T)(implicit ctx: Parse[_]): Parse[T] = ctx.freshSuccess(v)

  def Fail(implicit ctx: Parse[_]): Parse[Nothing] = ctx.freshFailure().asInstanceOf[Parse[Nothing]]

  def Index(implicit ctx: Parse[_]): Parse[Int] = ctx.freshSuccess(ctx.index)

  def AnyChar(implicit ctx: Parse[_]): Parse[Unit] = {
    if (!(ctx.index < ctx.input.length)) ctx.freshFailure().asInstanceOf[Parse[Unit]]
    else ctx.freshSuccess((), ctx.index + 1)
  }
  def SingleChar(implicit ctx: Parse[_]): Parse[Char] = {
    if (!(ctx.index < ctx.input.length)) ctx.freshFailure().asInstanceOf[Parse[Char]]
    else ctx.freshSuccess(ctx.input(ctx.index), ctx.index + 1)
  }
  def CharPred(p: Char => Boolean)(implicit ctx: Parse[_]): Parse[Unit] = {
    if (!(ctx.index < ctx.input.length && p(ctx.input(ctx.index)))) ctx.freshFailure().asInstanceOf[Parse[Unit]]
    else ctx.freshSuccess((), ctx.index + 1)
  }
  def CharsWhile(p: Char => Boolean, min: Int = 1)(implicit ctx: Parse[_]): Parse[Unit] = {
    var index = ctx.index
    val input = ctx.input
    val inputLength = input.length


    val start = index
    while(index < inputLength && p(input(index))) index += 1
    if (index - start >= min) ctx.freshSuccess((), index = index)
    else {
      ctx.isSuccess = false
      ctx.asInstanceOf[Parse[Unit]]
    }
  }

  def NoCut[T](parse: => Parse[T])(implicit ctx: Parse[_]): Parse[T] = {

    val oldNoCut = ctx.noCut
    val res = parse
    ctx.noCut = oldNoCut
    if (res.isSuccess) res.successCut = false
    else res.failureCut = false
    res
  }

  def parseInputCtx(s: String): Parse[_] = Parse(s)
}

