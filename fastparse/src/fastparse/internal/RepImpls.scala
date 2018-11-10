package fastparse.internal

import fastparse.{Implicits, NoWhitespace, ParsingRun}

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
object MacroRepImpls{
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
    val originalCut = TermName(c.freshName("originalCut"))
    val parsedMsg = TermName(c.freshName("parsedMsg"))
    val startAggregate = TermName(c.freshName("startAggregate"))
    val startGroup = TermName(c.freshName("startGroup"))
    val (endSnippet, aggregateSnippet) = min match{
      case None =>
        q"""
          $ctx1.freshSuccess($repeater1.result($acc), $startIndex, $originalCut)
        """ ->
        q""" "" """
      case Some(min1) =>
        q"""
           if ($count < $min1) $ctx1.augmentFailure($startIndex, $originalCut)
           else $ctx1.freshSuccess($repeater1.result($acc), $startIndex, $originalCut)
        """ ->
        q"""if($min1 == 0) "" else "(" + $min1 + ")""""
    }

    val wsSnippet = whitespace match{
      case None => q"$rec($beforeSepIndex, $count + 1, false)"
      case Some(ws) =>
        q"""
        if ($ws ne _root_.fastparse.NoWhitespace.noWhitespaceImplicit) {
          val oldNoDropBuffer = $ctx1.noDropBuffer // completely disallow dropBuffer
          $ctx1.noDropBuffer = true
          $ws($ctx1)
          $ctx1.noDropBuffer = oldNoDropBuffer
        }
        if (!$ctx1.isSuccess && $ctx1.cut) $ctx1.asInstanceOf[_root_.fastparse.ParsingRun[scala.Nothing]]
        else{
          $ctx1.cut = false
          $rec($beforeSepIndex, $count + 1, false)
        }
        """
    }

    q"""
      $ctx match{ case $ctx1 =>
        $repeater match {case $repeater1 =>
          var $originalCut = $ctx1.cut
          val $acc = $repeater1.initial
          @_root_.scala.annotation.tailrec
          def $rec($startIndex: _root_.scala.Int,
                   $count: _root_.scala.Int,
                   $precut: _root_.scala.Boolean): _root_.fastparse.P[${c.weakTypeOf[V]}] = {
            $ctx1.cut = $precut
            val $startGroup = $ctx1.failureGroupAggregate
            ${c.prefix}.parse0()
            val $startAggregate = $ctx1.earliestAggregate
            val $parsedMsg = $ctx1.shortParserMsg
            $originalCut |= $ctx1.cut
            if (!$ctx1.isSuccess) {
              val res =
                if ($ctx1.cut) $ctx1.asInstanceOf[_root_.fastparse.P[${c.weakTypeOf[V]}]]
                else $endSnippet
              if ($ctx1.verboseFailures) {
                if ($ctx1.index == $ctx1.traceIndex) $ctx1.failureGroupAggregate = $ctx1.shortParserMsg :: $startGroup
                $ctx1.aggregateTerminalPostBacktrack($startAggregate, () => $parsedMsg() + ".rep" + $aggregateSnippet)
              }
              res
            }else {
              val $beforeSepIndex = $ctx1.index
              $repeater1.accumulate($ctx1.successValue.asInstanceOf[${c.weakTypeOf[T]}], $acc)
              $ctx1.cut = false
              $wsSnippet

            }
          }
          $rec($ctx1.index, 0, false)
        }
      }
    """
  }

  def repXMacro1[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                    (repeater: c.Tree,
                                                     ctx: c.Tree): c.Tree = {
    import c.universe._
    MacroRepImpls.repXMacro0[T, V](c)(None, None)(repeater, ctx)
  }

  def repXMacro2[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                    (min: c.Tree)
                                                    (repeater: c.Tree,
                                                     ctx: c.Tree): c.Tree = {
    import c.universe._
    MacroRepImpls.repXMacro0[T, V](c)(None, Some(min))(repeater, ctx)
  }

  def repXMacro1ws[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                      (repeater: c.Tree,
                                                       whitespace: c.Tree,
                                                       ctx: c.Tree): c.Tree = {
    import c.universe._
    MacroRepImpls.repXMacro0[T, V](c)(Some(whitespace), None)(repeater, ctx)
  }

  def repXMacro2ws[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                                                      (min: c.Tree)
                                                      (repeater: c.Tree,
                                                       whitespace: c.Tree,
                                                       ctx: c.Tree): c.Tree = {
    import c.universe._
    MacroRepImpls.repXMacro0[T, V](c)(Some(whitespace), Some(min))(repeater, ctx)
  }
}

class RepImpls[T](val parse0: () => ParsingRun[T]) extends AnyVal{
  def repX[V](min: Int = 0,
              sep: => ParsingRun[_] = null,
              max: Int = Int.MaxValue,
              exactly: Int = -1)
             (implicit repeater: Implicits.Repeater[T, V],
              ctx: ParsingRun[Any]): ParsingRun[V] = {

    val acc = repeater.initial
    val actualMin = if(exactly == -1) min else exactly
    val actualMax = if(exactly == -1) max else exactly
    var originalCut = ctx.cut
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < actualMin) ctx.augmentFailure(index, originalCut)
      else ctx.freshSuccess(repeater.result(acc), successIndex, originalCut)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean, preSep: Boolean): ParsingRun[V] = {
      ctx.cut = precut
      if (count == 0 && actualMax == 0) ctx.freshSuccess(repeater.result(acc), startIndex)
      else {
        val startGroup = ctx.failureGroupAggregate
        parse0()
        val earliestAggregated = ctx.earliestAggregate
        val parseMsg = ctx.shortParserMsg
        originalCut |= ctx.cut
        if (!ctx.isSuccess) {
          val res =
            if (ctx.cut) ctx.asInstanceOf[ParsingRun[V]]
            else end(startIndex, startIndex, count)

          if (ctx.verboseFailures && !preSep) {
            if (ctx.index == ctx.traceIndex) ctx.failureGroupAggregate = ctx.shortParserMsg :: startGroup
            ctx.aggregateTerminalPostBacktrack(earliestAggregated, () => parseMsg() + ".repX" + (if(min == 0) "" else s"($min)"))
          }
          res
        }else {
          val beforeSepIndex = ctx.index
          repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
          val nextCount = count + 1
          if (nextCount == actualMax) {
            val res = end(beforeSepIndex, beforeSepIndex, nextCount)
            if (ctx.verboseFailures) ctx.setMsg(() => parseMsg() + ".repX" + (if(min == 0) "" else s"($min)"))
            res
          }
          else {
            ctx.cut = false

            val sep1 = sep
            originalCut |= ctx.cut
            if (sep1 == null) rec(beforeSepIndex, nextCount, false, false)
            else {
              if (ctx.isSuccess) rec(beforeSepIndex, nextCount, ctx.cut, true)
              else {
                val res =
                  if (ctx.cut) ctx.augmentFailure(beforeSepIndex, originalCut)
                  else end(beforeSepIndex, beforeSepIndex, nextCount)
                if (ctx.verboseFailures) {
                  if (ctx.index == ctx.traceIndex) ctx.failureGroupAggregate ::= ctx.shortParserMsg
                  ctx.setMsg(() => parseMsg() + ".repX" + (if(min == 0) "" else s"($min)"))
                }
                res
              }
            }
          }
        }
      }
    }
    rec(ctx.index, 0, false, false)
  }
  def repX[V](min: Int,
              sep: => ParsingRun[_])
             (implicit repeater: Implicits.Repeater[T, V],
              ctx: ParsingRun[Any]): ParsingRun[V] = {
    var originalCut = ctx.cut
    val acc = repeater.initial
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < min) ctx.augmentFailure(index, originalCut)
      else ctx.freshSuccess(repeater.result(acc), successIndex, originalCut)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean, preSep: Boolean): ParsingRun[V] = {
      ctx.cut = precut
      val startGroup = ctx.failureGroupAggregate
      parse0()
      val earliestAggregated = ctx.earliestAggregate
      val parsedMsg = ctx.shortParserMsg
      originalCut |= ctx.cut
      if (!ctx.isSuccess) {
        val res =
          if (ctx.cut) ctx.asInstanceOf[ParsingRun[V]]
          else end(startIndex, startIndex, count)
        if (ctx.verboseFailures && !preSep) {
          if (ctx.index == ctx.traceIndex) ctx.failureGroupAggregate = ctx.shortParserMsg :: startGroup
          ctx.aggregateTerminalPostBacktrack(earliestAggregated, () => parsedMsg() + s".repX($min)")
        }
        res
      }else {
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        ctx.cut = false
        val sep1 = sep
        originalCut |= ctx.cut
        if (sep1 == null) rec(beforeSepIndex, nextCount, false, false)
        else {
          if (ctx.isSuccess) rec(beforeSepIndex, nextCount, ctx.cut, true)
          else {
            val res =
              if (ctx.cut) ctx.augmentFailure(beforeSepIndex, ctx.cut | originalCut)
              else end(beforeSepIndex, beforeSepIndex, nextCount)
            if (ctx.verboseFailures) {
              if (ctx.index == ctx.traceIndex) ctx.failureGroupAggregate ::= ctx.shortParserMsg
              ctx.setMsg(() => parsedMsg() + s".repX($min)")
            }
            res
          }
        }
      }
    }
    rec(ctx.index, 0, false, false)
  }
  def rep[V](min: Int = 0,
             sep: => ParsingRun[_] = null,
             max: Int = Int.MaxValue,
             exactly: Int = -1)
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: ParsingRun[_] => ParsingRun[Unit],
             ctx: ParsingRun[Any]): ParsingRun[V] = {

    var originalCut = ctx.cut
    val acc = repeater.initial
    val actualMin = if(exactly == -1) min else exactly
    val actualMax = if(exactly == -1) max else exactly
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < actualMin) ctx.augmentFailure(index, originalCut)
      else ctx.freshSuccess(repeater.result(acc), successIndex, originalCut)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean, preSep: Boolean): ParsingRun[V] = {
      ctx.cut = precut
      if (count == 0 && actualMax == 0) ctx.freshSuccess(repeater.result(acc), startIndex)
      else {
        val startGroup = ctx.failureGroupAggregate
        parse0()
        val earliestAggregated = ctx.earliestAggregate
        val parsedMsg = ctx.shortParserMsg
        originalCut |= ctx.cut
        if (!ctx.isSuccess) {
          val res =
            if (ctx.cut) ctx.asInstanceOf[ParsingRun[V]]
            else end(startIndex, startIndex, count)
          if (ctx.verboseFailures && !preSep) {
            if (ctx.index == ctx.traceIndex) ctx.failureGroupAggregate = ctx.shortParserMsg :: startGroup
            ctx.aggregateTerminalPostBacktrack(earliestAggregated, () => parsedMsg() + ".rep" + (if(min == 0) "" else s"($min)"))
          }
          res
        } else {
          val beforeSepIndex = ctx.index
          repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
          val nextCount = count + 1
          if (nextCount == actualMax) {
            val res = end(beforeSepIndex, beforeSepIndex, nextCount)
            if (ctx.verboseFailures) ctx.setMsg(() => parsedMsg() + ".rep" + (if(min == 0) "" else s"($min)"))
            res
          }
          else {
            if (whitespace ne NoWhitespace.noWhitespaceImplicit) whitespace(ctx)
            if (!ctx.isSuccess && ctx.cut) ctx.asInstanceOf[ParsingRun[Nothing]]
            else {
              ctx.cut = false
              val sep1 = sep
              originalCut |= ctx.cut
              if (sep1 == null) rec(beforeSepIndex, nextCount, false, false)
              else if (ctx.isSuccess) {
                val sepCut = ctx.cut
                if (whitespace ne NoWhitespace.noWhitespaceImplicit) whitespace(ctx)
                if (!ctx.isSuccess && ctx.cut) ctx.asInstanceOf[ParsingRun[Nothing]]
                else rec(beforeSepIndex, nextCount, sepCut, true)
              }
              else {
                val res =
                  if (ctx.cut) ctx.augmentFailure(beforeSepIndex, originalCut)
                  else end(beforeSepIndex, beforeSepIndex, nextCount)
                if (ctx.verboseFailures) {
                  if (ctx.index == ctx.traceIndex) ctx.failureGroupAggregate = ctx.shortParserMsg :: startGroup
                  ctx.setMsg(() => parsedMsg() + ".rep" + (if(min == 0) "" else s"($min)"))
                }
                res
              }
            }
          }
        }
      }
    }
    rec(ctx.index, 0, false, false)
  }
  def rep[V](min: Int,
             sep: => ParsingRun[_])
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: ParsingRun[_] => ParsingRun[Unit],
             ctx: ParsingRun[Any]): ParsingRun[V] = {

    var originalCut = ctx.cut
    val acc = repeater.initial
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < min) ctx.augmentFailure(index, originalCut)
      else ctx.freshSuccess(repeater.result(acc), successIndex, originalCut)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean, preSep: Boolean): ParsingRun[V] = {
      ctx.cut = precut
      val earliestAggregated = ctx.earliestAggregate
      val startGroup = ctx.failureGroupAggregate
      parse0()
      val parsedMsg = ctx.shortParserMsg
      originalCut |= ctx.cut
      if (!ctx.isSuccess){
        val res =
          if (ctx.cut) ctx.asInstanceOf[ParsingRun[V]]
          else end(startIndex, startIndex, count)
        if (ctx.verboseFailures && !preSep) {
          if (ctx.index == ctx.traceIndex) ctx.failureGroupAggregate = ctx.shortParserMsg :: startGroup
          ctx.aggregateTerminalPostBacktrack(earliestAggregated, () => parsedMsg() + s".rep($min)")
        }
        res
      }else{
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        if (whitespace ne NoWhitespace.noWhitespaceImplicit) {
          val oldCapturing = ctx.noDropBuffer // completely disallow dropBuffer
          ctx.noDropBuffer = true
          whitespace(ctx)
          ctx.noDropBuffer = oldCapturing
        }
        if (!ctx.isSuccess && ctx.cut) ctx.asInstanceOf[ParsingRun[Nothing]]
        else {
          ctx.cut = false
          val sep1 = sep
          originalCut |= ctx.cut
          if (sep1 == null) rec(beforeSepIndex, nextCount, false, false)
          else if (ctx.isSuccess) {
            val sepCut = ctx.cut
            if (whitespace ne NoWhitespace.noWhitespaceImplicit) {
              val oldCapturing = ctx.noDropBuffer // completely disallow dropBuffer
              ctx.noDropBuffer = true
              whitespace(ctx)
              ctx.noDropBuffer = oldCapturing
            }
            rec(beforeSepIndex, nextCount, sepCut, true)
          }
          else {
            val res =
              if (ctx.cut) ctx.augmentFailure(beforeSepIndex, ctx.cut | originalCut)
              else end(beforeSepIndex, beforeSepIndex, nextCount)
            if (ctx.verboseFailures) {
              if (ctx.index == ctx.traceIndex) ctx.failureGroupAggregate = ctx.shortParserMsg :: startGroup
              ctx.setMsg(() => parsedMsg() + s".rep($min)")
            }
            res
          }
        }
      }
    }
    rec(ctx.index, 0, false, false)
  }

}
