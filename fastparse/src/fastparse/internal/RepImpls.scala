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
    val beforeSepIndex = TermName(c.freshName("beforeSepIndex"))
    val rec = TermName(c.freshName("rec"))
    val originalCut = TermName(c.freshName("originalCut"))
    val parsedMsg = TermName(c.freshName("parsedMsg"))
    val lastAgg = TermName(c.freshName("lastAgg"))
    val parsedAgg = TermName(c.freshName("parsedAgg"))
    val ((endSnippet, aggregateSnippet), minCut) = min match{
      case None =>
        q"""
          $ctx1.freshSuccess($repeater1.result($acc), $startIndex, $originalCut)
        """ ->
        q""" "" """ ->
        q"""false"""
      case Some(min1) =>
        q"""
           if ($count < $min1) $ctx1.augmentFailure($startIndex, $originalCut)
           else $ctx1.freshSuccess($repeater1.result($acc), $startIndex, $originalCut)
        """ ->
        q"""if($min1 == 0) "" else "(" + $min1 + ")"""" ->
        q"""$originalCut && ($count < $min1)"""
    }

    val wsSnippet = whitespace match{
      case None => q"$rec($beforeSepIndex, $count + 1, $parsedAgg)"
      case Some(ws) =>
        q"""
        if ($ws ne _root_.fastparse.NoWhitespace.noWhitespaceImplicit) {
           _root_.fastparse.internal.Util.consumeWhitespace($ws, $ctx1)
        }
        if (!$ctx1.isSuccess && $ctx1.cut) $ctx1.asInstanceOf[_root_.fastparse.ParsingRun[scala.Nothing]]
        else{
          $ctx1.cut = false
          $rec($beforeSepIndex, $count + 1, $parsedAgg)
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
                   $lastAgg: _root_.fastparse.internal.Msgs): _root_.fastparse.P[${c.weakTypeOf[V]}] = {
            $ctx1.cut = $minCut
            ${c.prefix}.parse0()

            val $parsedMsg = $ctx1.shortParserMsg
            val $parsedAgg = $ctx1.failureGroupAggregate
            $originalCut |= $ctx1.cut
            if (!$ctx1.isSuccess) {
              val res =
                if ($ctx1.cut) $ctx1.asInstanceOf[_root_.fastparse.P[${c.weakTypeOf[V]}]]
                else $endSnippet
              if ($ctx1.verboseFailures) {
                $ctx1.aggregateMsg(
                  $startIndex,
                  () => $parsedMsg.render + s".rep" + $aggregateSnippet,
                  if ($lastAgg == null) $ctx1.failureGroupAggregate
                  else $ctx1.failureGroupAggregate ::: $lastAgg
                )
              }
              res
            }else {
              val $beforeSepIndex = $ctx1.index
              $repeater1.accumulate($ctx1.successValue.asInstanceOf[${c.weakTypeOf[T]}], $acc)
              $ctx1.cut = false
              $wsSnippet
            }
          }
          $rec($ctx1.index, 0, null)
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

    def end(successIndex: Int, index: Int, count: Int, endCut: Boolean) = {
      if (count < actualMin) ctx.augmentFailure(index, endCut)
      else ctx.freshSuccess(repeater.result(acc), successIndex, endCut)
    }
    @tailrec def rec(startIndex: Int,
                     count: Int,
                     precut: Boolean,
                     outerCut: Boolean,
                     sepMsg: Msgs,
                     lastAgg: Msgs): ParsingRun[V] = {
      ctx.cut = precut | (count < min && outerCut)
      if (count == 0 && actualMax == 0) ctx.freshSuccess(repeater.result(acc), startIndex)
      else {
        val verboseFailures = ctx.verboseFailures
        parse0()
        val parsedMsg = ctx.shortParserMsg
        val parsedAgg = ctx.failureGroupAggregate
        val postCut = ctx.cut
        if (!ctx.isSuccess) {
          val res =
            if (postCut) ctx.asInstanceOf[ParsingRun[V]]
            else end(startIndex, startIndex, count, outerCut | postCut)

          if (verboseFailures) aggregateMsgInRep(startIndex, min, ctx, sepMsg, parsedMsg, lastAgg, precut)
          res
        }else {
          val beforeSepIndex = ctx.index
          repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
          val nextCount = count + 1
          if (nextCount == actualMax) {
            val res = end(beforeSepIndex, beforeSepIndex, nextCount, outerCut | postCut)
            if (verboseFailures) ctx.setMsg(startIndex, () => parsedMsg.render + ".repX" + (if(min == 0) "" else s"($min)"))
            res
          }
          else {
            ctx.cut = false
            val sep1 = sep
            val sepCut = ctx.cut
            val endCut = outerCut | postCut | sepCut
            if (sep1 == null) rec(beforeSepIndex, nextCount, false, endCut, null, parsedAgg)
            else {
              if (ctx.isSuccess) rec(beforeSepIndex, nextCount, sepCut, endCut, ctx.shortParserMsg, parsedAgg)
              else {
                val res =
                  if (sepCut) ctx.augmentFailure(beforeSepIndex, endCut)
                  else end(beforeSepIndex, beforeSepIndex, nextCount, endCut)
                if (verboseFailures) aggregateMsgPostSep(startIndex, min, ctx, parsedMsg, parsedAgg)
                res
              }
            }
          }
        }
      }
    }
    rec(ctx.index, 0, false, ctx.cut, null, null)
  }

  def repX[V](min: Int,
              sep: => ParsingRun[_])
             (implicit repeater: Implicits.Repeater[T, V],
              ctx: ParsingRun[Any]): ParsingRun[V] = {

    val acc = repeater.initial

    def end(successIndex: Int, index: Int, count: Int, endCut: Boolean) = {
      if (count < min) ctx.augmentFailure(index, endCut)
      else ctx.freshSuccess(repeater.result(acc), successIndex, endCut)
    }
    @tailrec def rec(startIndex: Int,
                     count: Int,
                     precut: Boolean,
                     outerCut: Boolean,
                     sepMsg: Msgs,
                     lastAgg: Msgs): ParsingRun[V] = {
      ctx.cut = precut | (count < min && outerCut)
      parse0()
      val parsedMsg = ctx.shortParserMsg
      val parsedAgg = ctx.failureGroupAggregate
      val postCut = ctx.cut
      val verboseFailures = ctx.verboseFailures
      if (!ctx.isSuccess) {
        val res =
          if (postCut) ctx.asInstanceOf[ParsingRun[V]]
          else end(startIndex, startIndex, count, outerCut | postCut)
        if (verboseFailures) aggregateMsgInRep(startIndex, min, ctx, sepMsg, parsedMsg, lastAgg, precut)
        res
      }else {
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        ctx.cut = false
        val sep1 = sep
        val sepCut = ctx.cut
        val endCut = outerCut | postCut | sepCut
        if (sep1 == null) rec(beforeSepIndex, nextCount, false, endCut, null, parsedAgg)
        else {
          if (ctx.isSuccess) rec(beforeSepIndex, nextCount, sepCut, endCut, ctx.shortParserMsg, parsedAgg)
          else {
            val res =
              if (sepCut) ctx.augmentFailure(beforeSepIndex, endCut)
              else end(beforeSepIndex, beforeSepIndex, nextCount, endCut)
            if (verboseFailures) aggregateMsgPostSep(startIndex, min, ctx, parsedMsg, parsedAgg)
            res
          }
        }
      }
    }
    rec(ctx.index, 0, false, ctx.cut, null, null)
  }
  def rep[V](min: Int = 0,
             sep: => ParsingRun[_] = null,
             max: Int = Int.MaxValue,
             exactly: Int = -1)
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: ParsingRun[_] => ParsingRun[Unit],
             ctx: ParsingRun[Any]): ParsingRun[V] = {

    val acc = repeater.initial
    val actualMin = if(exactly == -1) min else exactly
    val actualMax = if(exactly == -1) max else exactly

    def end(successIndex: Int, index: Int, count: Int, endCut: Boolean) = {
      if (count < actualMin) ctx.augmentFailure(index, endCut)
      else ctx.freshSuccess(repeater.result(acc), successIndex, endCut)
    }
    @tailrec def rec(startIndex: Int,
                     count: Int,
                     precut: Boolean,
                     outerCut: Boolean,
                     sepMsg: Msgs,
                     lastAgg: Msgs): ParsingRun[V] = {
      ctx.cut = precut | (count < min && outerCut)
      if (count == 0 && actualMax == 0) ctx.freshSuccess(repeater.result(acc), startIndex)
      else {
        parse0()
        val parsedMsg = ctx.shortParserMsg
        val parsedAgg = ctx.failureGroupAggregate
        val postCut = ctx.cut
        val verboseFailures = ctx.verboseFailures
        if (!ctx.isSuccess) {
          val res =
            if (postCut) ctx.asInstanceOf[ParsingRun[V]]
            else end(startIndex, startIndex, count, outerCut | postCut)
          if (verboseFailures) aggregateMsgInRep(startIndex, min, ctx, sepMsg, parsedMsg, lastAgg, precut)
          res
        } else {
          val beforeSepIndex = ctx.index
          repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
          val nextCount = count + 1
          if (nextCount == actualMax) {
            val res = end(beforeSepIndex, beforeSepIndex, nextCount, outerCut | postCut)
            if (verboseFailures) ctx.setMsg(startIndex, () => parsedMsg.render + ".rep" + (if(min == 0) "" else s"($min)"))
            res
          }
          else {
            if (whitespace ne NoWhitespace.noWhitespaceImplicit) Util.consumeWhitespace(whitespace, ctx)

            if (!ctx.isSuccess && ctx.cut) ctx.asInstanceOf[ParsingRun[Nothing]]
            else {
              ctx.cut = false
              val sep1 = sep
              val sepCut = ctx.cut
              val endCut = outerCut | postCut | sepCut
              if (sep1 == null) rec(beforeSepIndex, nextCount, false, endCut, null, parsedAgg)
              else if (ctx.isSuccess) {
                if (whitespace ne NoWhitespace.noWhitespaceImplicit) Util.consumeWhitespace(whitespace, ctx)
                if (!ctx.isSuccess && sepCut) ctx.asInstanceOf[ParsingRun[Nothing]]
                else rec(beforeSepIndex, nextCount, sepCut, endCut, ctx.shortParserMsg, parsedAgg)
              }
              else {
                val res =
                  if (sepCut) ctx.augmentFailure(beforeSepIndex, endCut)
                  else end(beforeSepIndex, beforeSepIndex, nextCount, endCut)

                if (verboseFailures) aggregateMsgPostSep(startIndex, min, ctx, parsedMsg, parsedAgg)
                res
              }
            }
          }
        }
      }
    }
    rec(ctx.index, 0, false, ctx.cut, null, null)
  }
  def rep[V](min: Int,
             sep: => ParsingRun[_])
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: ParsingRun[_] => ParsingRun[Unit],
             ctx: ParsingRun[Any]): ParsingRun[V] = {

    val acc = repeater.initial

    def end(successIndex: Int, index: Int, count: Int, endCut: Boolean) = {
      if (count < min) ctx.augmentFailure(index, endCut)
      else ctx.freshSuccess(repeater.result(acc), successIndex, endCut)
    }
    @tailrec def rec(startIndex: Int,
                     count: Int,
                     precut: Boolean,
                     outerCut: Boolean,
                     sepMsg: Msgs,
                     lastAgg: Msgs): ParsingRun[V] = {

      ctx.cut = precut | (count < min && outerCut)
      parse0()
      val parsedMsg = ctx.shortParserMsg
      val parsedAgg = ctx.failureGroupAggregate
      val postCut = ctx.cut
      val verboseFailures = ctx.verboseFailures
      if (!ctx.isSuccess){
        val res =
          if (postCut) ctx.asInstanceOf[ParsingRun[V]]
          else end(startIndex, startIndex, count, outerCut | postCut)
        if (verboseFailures) aggregateMsgInRep(startIndex, min, ctx, sepMsg, parsedMsg, lastAgg, precut)
        res
      }else{
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        if (whitespace ne NoWhitespace.noWhitespaceImplicit) Util.consumeWhitespace(whitespace, ctx)

        if (!ctx.isSuccess && ctx.cut) ctx.asInstanceOf[ParsingRun[Nothing]]
        else {
          ctx.cut = false
          val sep1 = sep
          val sepCut = ctx.cut
          val endCut = outerCut | postCut | sepCut
          if (sep1 == null) rec(beforeSepIndex, nextCount, false, endCut, null, parsedAgg)
          else if (ctx.isSuccess) {
            if (whitespace ne NoWhitespace.noWhitespaceImplicit) Util.consumeWhitespace(whitespace, ctx)

            rec(beforeSepIndex, nextCount, sepCut, endCut, ctx.shortParserMsg, parsedAgg)
          }
          else {
            val res =
              if (sepCut) ctx.augmentFailure(beforeSepIndex, endCut)
              else end(beforeSepIndex, beforeSepIndex, nextCount, endCut)

            if (verboseFailures) aggregateMsgPostSep(startIndex, min, ctx, parsedMsg, parsedAgg)
            res
          }
        }
      }
    }
    rec(ctx.index, 0, false, ctx.cut, null, null)
  }

  private def aggregateMsgPostSep[V](startIndex: Int,
                                     min: Int,
                                     ctx: ParsingRun[Any],
                                     parsedMsg: Msgs,
                                     lastAgg: Msgs) = {
    ctx.aggregateMsg(
      startIndex,
      () => parsedMsg.render + s".rep($min)",
      // When we fail on a sep, we collect the failure aggregate of the last
      // non-sep rep body together with the failure aggregate of the sep, since
      // the last non-sep rep body continuing is one of the valid ways of
      // continuing the parse
      ctx.failureGroupAggregate ::: lastAgg

    )
  }

  private def aggregateMsgInRep[V](startIndex: Int,
                                   min: Int,
                                   ctx: ParsingRun[Any],
                                   sepMsg: Msgs,
                                   parsedMsg: Msgs,
                                   lastAgg: Msgs,
                                   precut: Boolean) = {
    if (sepMsg == null || precut) {
      ctx.aggregateMsg(
        startIndex,
        () => parsedMsg.render + s".rep($min)",
        ctx.failureGroupAggregate
      )
    } else {
      ctx.aggregateMsg(
        startIndex,
        () => parsedMsg.render + s".rep($min)",
        // When we fail on a rep body, we collect both the concatenated
        // sep and failure aggregate  of the rep body that we tried (because
        // we backtrack past the sep on failure) as well as the failure
        // aggregate of the previous rep, which we could have continued
        if (lastAgg == null) Util.joinBinOp(sepMsg, parsedMsg)
        else Util.joinBinOp(sepMsg, parsedMsg)  ::: lastAgg
      )
    }
  }

}
