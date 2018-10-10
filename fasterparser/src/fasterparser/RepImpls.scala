package fasterparser

import scala.annotation.tailrec
import scala.reflect.macros.blackbox.Context
import language.experimental.macros

/**
  * Implementations of the various `.rep`/`.repX` overloads. The most common
  * and simple overloads are implemented as macros for performance, while the
  * more complex/general cases are left as normal methods to avoid code bloat
  * and allow the use of default/named arguments (which don't work in macros
  * due to https://github.com/scala/bug/issues/5920).
  *
  * Even the normal method overloads are manually-specialized to some extent
  * for various sorts of inputs as a best-effort attempt ot minimize branching
  * in the hot paths.
  */
object RepImpls{
  def repXMacro0[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                    (whitespace: Option[c.Tree], min: Option[c.Tree])
                                                    (repeater: c.Tree,
                                                     ctx: c.Tree): c.Tree = {
    import c.universe._
    val repeater1 = TermName(c.freshName("repeater"))
    val ctx1 = TermName(c.freshName("repeater"))
    val acc = TermName(c.freshName("acc"))
    val startIndex = TermName(c.freshName("startIndex"))
    val count = TermName(c.freshName("count"))
    val precut = TermName(c.freshName("precut"))
    val beforeSepIndex = TermName(c.freshName("beforeSepIndex"))
    val rec = TermName(c.freshName("rec"))
    val endSnippet = min match{
      case None =>
        q"""
          $ctx1.prepareSuccess($repeater1.result($acc), $startIndex)
        """
      case Some(min1) =>
        q"""
           if ($count < $min1) {
             $ctx1.prepareFailure($startIndex)
           }else $ctx1.prepareSuccess($repeater1.result($acc), $startIndex)
        """
    }

    val wsSnippet = whitespace match{
      case None => q"()"
      case Some(ws) =>
        q"""
        if ($ws ne fasterparser.NoWhitespace.noWhitespaceImplicit) {
          val oldCapturing = $ctx1.isCapturing // completely disallow dropBuffer
          $ctx1.isCapturing = true
          $ws($ctx1)
          $ctx1.isCapturing = oldCapturing
        }
        $ctx1.cut = false
        """
    }

    q"""
      $ctx match{ case $ctx1 =>
        val $repeater1 = $repeater
        val $acc = $repeater1.initial
        @_root_.scala.annotation.tailrec
        def $rec($startIndex: _root_.scala.Int,
                 $count: _root_.scala.Int,
                 $precut: _root_.scala.Boolean): _root_.fasterparser.Parse[${c.weakTypeOf[V]}] = {
          $ctx1.cut = $precut

          val oldIsFork = $ctx.isFork
          $ctx1.isFork = true
          ${c.prefix}.parse0()
          $ctx1.isFork = oldIsFork
          if (!$ctx1.isSuccess) {
            if ($ctx1.cut | $precut) $ctx1.asInstanceOf[Parse[${c.weakTypeOf[V]}]]
            else $endSnippet
          }else {
            val $beforeSepIndex = $ctx1.index
            $repeater1.accumulate($ctx1.successValue.asInstanceOf[${c.weakTypeOf[T]}], $acc)
            $ctx1.cut = false
            $wsSnippet
            $rec($beforeSepIndex, $count + 1, false)
          }
        }
        $ctx1.isSuccess = true
        $rec($ctx1.index, 0, false)
      }
    """
  }

  def repXMacro1[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                    (repeater: c.Tree,
                                                     ctx: c.Tree): c.Tree = {
    import c.universe._
    RepImpls.repXMacro0[T, V](c)(None, None)(repeater, ctx)
  }

  def repXMacro2[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                    (min: c.Tree)
                                                    (repeater: c.Tree,
                                                     ctx: c.Tree): c.Tree = {
    import c.universe._
    RepImpls.repXMacro0[T, V](c)(None, Some(min))(repeater, ctx)
  }

  def repXMacro1ws[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                      (repeater: c.Tree,
                                                       whitespace: c.Tree,
                                                       ctx: c.Tree): c.Tree = {
    import c.universe._
    RepImpls.repXMacro0[T, V](c)(Some(whitespace), None)(repeater, ctx)
  }

  def repXMacro2ws[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                      (min: c.Tree)
                                                      (repeater: c.Tree,
                                                       whitespace: c.Tree,
                                                       ctx: c.Tree): c.Tree = {
    import c.universe._
    RepImpls.repXMacro0[T, V](c)(Some(whitespace), Some(min))(repeater, ctx)
  }
}

class RepImpls[T](val parse0: () => Parse[T]) extends AnyVal{
  def repX[V](min: Int = 0,
              sep: => Parse[_] = null,
              max: Int = Int.MaxValue,
              exactly: Int = -1)
             (implicit repeater: Implicits.Repeater[T, V],
              ctx: Parse[Any]): Parse[V] = {

    val acc = repeater.initial
    val actualMin = if(exactly == -1) min else exactly
    val actualMax = if(exactly == -1) max else exactly
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < actualMin) ctx.prepareFailure(index)
      else ctx.prepareSuccess(repeater.result(acc), successIndex)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
      ctx.cut = precut
      if (count == 0 && actualMax == 0) ctx.prepareSuccess(repeater.result(acc), startIndex)
      else {
        val oldIsFork = ctx.isFork
        ctx.isFork = true
        parse0()
        ctx.isFork = oldIsFork
        if (!ctx.isSuccess) {
          if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
          else end(startIndex, startIndex, count)
        }else {
          val beforeSepIndex = ctx.index
          repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
          val nextCount = count + 1
          if (nextCount == actualMax) end(beforeSepIndex, beforeSepIndex, nextCount)
          else {
            ctx.cut = false

            ctx.isFork = true
            val sep1 = sep
            ctx.isFork = oldIsFork
            if (sep1 == null) rec(beforeSepIndex, nextCount, false)
            else {
              if (ctx.isSuccess) rec(beforeSepIndex, nextCount, ctx.cut)
              else if (ctx.cut) ctx.prepareFailure(beforeSepIndex)
              else end(beforeSepIndex, beforeSepIndex, nextCount)
            }
          }
        }
      }
    }
    ctx.isSuccess = true
    rec(ctx.index, 0, false)
  }
  def repX[V](min: Int,
              sep: => Parse[_])
             (implicit repeater: Implicits.Repeater[T, V],
              ctx: Parse[Any]): Parse[V] = {

    val acc = repeater.initial
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < min) ctx.prepareFailure(index)
      else ctx.prepareSuccess(repeater.result(acc), successIndex)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
      ctx.cut = precut
      val oldIsFork = ctx.isFork
      ctx.isFork = true
      parse0()
      ctx.isFork = oldIsFork
      if (!ctx.isSuccess) {
        if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
        else end(startIndex, startIndex, count)
      }else {
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        ctx.cut = false
        ctx.isFork = true
        val sep1 = sep
        ctx.isFork = oldIsFork
        if (sep1 == null) rec(beforeSepIndex, nextCount, false)
        else {
          if (ctx.isSuccess) rec(beforeSepIndex, nextCount, ctx.cut)
          else if (ctx.cut) ctx.prepareFailure(beforeSepIndex)
          else end(beforeSepIndex, beforeSepIndex, nextCount)
        }
      }
    }
    ctx.isSuccess = true
    rec(ctx.index, 0, false)
  }
  def rep[V](min: Int = 0,
             sep: => Parse[_] = null,
             max: Int = Int.MaxValue,
             exactly: Int = -1)
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: Parse[_] => Parse[Unit],
             ctx: Parse[Any]): Parse[V] = {


    val acc = repeater.initial
    val actualMin = if(exactly == -1) min else exactly
    val actualMax = if(exactly == -1) max else exactly
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < actualMin) ctx.prepareFailure(index)
      else ctx.prepareSuccess(repeater.result(acc), successIndex)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
      ctx.cut = precut
      if (count == 0 && actualMax == 0) ctx.prepareSuccess(repeater.result(acc), startIndex)
      else {
        val oldIsFork = ctx.isFork
        ctx.isFork = true
        parse0()
        ctx.isFork = oldIsFork
        if (!ctx.isSuccess){
          if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
          else end(startIndex, startIndex, count)
        }else{
          val beforeSepIndex = ctx.index
          repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
          val nextCount = count + 1
          if (nextCount == actualMax) end(beforeSepIndex, beforeSepIndex, nextCount)
          else {
            if (whitespace ne NoWhitespace.noWhitespaceImplicit) whitespace(ctx)
            ctx.cut = false
            ctx.isFork = true
            val sep1 = sep
            ctx.isFork = oldIsFork
            if (sep1 == null) rec(beforeSepIndex, nextCount, false)
            else if (ctx.isSuccess) {
              val sepCut = ctx.cut
              if (whitespace ne NoWhitespace.noWhitespaceImplicit) whitespace(ctx)
              rec(beforeSepIndex, nextCount, sepCut)
            }
            else if (ctx.cut) ctx.prepareFailure(beforeSepIndex)
            else end(beforeSepIndex, beforeSepIndex, nextCount)
          }
        }
      }
    }
    rec(ctx.index, 0, false)
  }
  def rep[V](min: Int,
             sep: => Parse[_])
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: Parse[_] => Parse[Unit],
             ctx: Parse[Any]): Parse[V] = {


    val acc = repeater.initial
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < min) ctx.prepareFailure(index)
      else ctx.prepareSuccess(repeater.result(acc), successIndex)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
      ctx.cut = precut
      val oldIsFork = ctx.isFork
      ctx.isFork = true
      parse0()
      ctx.isFork = oldIsFork
      if (!ctx.isSuccess){
        if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
        else end(startIndex, startIndex, count)
      }else{
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        if (whitespace ne NoWhitespace.noWhitespaceImplicit) {
          val oldCapturing = ctx.isCapturing // completely disallow dropBuffer
          ctx.isCapturing = true
          whitespace(ctx)
          ctx.isCapturing = oldCapturing
        }
        ctx.cut = false
        ctx.isFork = true
        val sep1 = sep
        ctx.isFork = oldIsFork
        if (sep1 == null) rec(beforeSepIndex, nextCount, false)
        else if (ctx.isSuccess) {
          val sepCut = ctx.cut
          if (whitespace ne NoWhitespace.noWhitespaceImplicit) {
            val oldCapturing = ctx.isCapturing // completely disallow dropBuffer
            ctx.isCapturing = true
            whitespace(ctx)
            ctx.isCapturing = oldCapturing
          }
          rec(beforeSepIndex, nextCount, sepCut)
        }
        else if (ctx.cut) ctx.prepareFailure(beforeSepIndex)
        else end(beforeSepIndex, beforeSepIndex, nextCount)
      }
    }
    rec(ctx.index, 0, false)
  }

}