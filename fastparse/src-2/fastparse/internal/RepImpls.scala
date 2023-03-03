package fastparse.internal


import fastparse.{Implicits, NoWhitespace, ParsingRun}
import Util.{aggregateMsgInRep, aggregateMsgPostSep}
import scala.annotation.tailrec


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
             whitespace: fastparse.Whitespace,
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
    }
    rec(ctx.index, 0, false, ctx.cut, null, null)
  }
  def rep[V](min: Int,
             sep: => ParsingRun[_])
            (implicit repeater: Implicits.Repeater[T, V],
             whitespace: fastparse.Whitespace,
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
    rec(ctx.index, 0, false, ctx.cut, null, null)
  }

}
