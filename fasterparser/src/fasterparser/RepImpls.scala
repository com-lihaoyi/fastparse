package fasterparser

import scala.annotation.tailrec

class RepImpls[T](val parse0: () => Parse[T]) extends AnyVal{

  def repX[V](implicit repeater: Implicits.Repeater[T, V], ctx: Parse[Any]): Parse[V] = {

    val acc = repeater.initial
    def end(successIndex: Int, index: Int, count: Int) = {
      ctx.prepareSuccess(repeater.result(acc), successIndex)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
      ctx.cut = precut
      parse0()
      if (!ctx.isSuccess) {
        if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
        else end(startIndex, startIndex, count)
      }else {
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        ctx.cut = false
        rec(beforeSepIndex, nextCount, false)
      }
    }
    ctx.isSuccess = true
    rec(ctx.index, 0, false)
  }
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
        parse0()
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
            if (sep == null) rec(beforeSepIndex, nextCount, false)
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
      parse0()
      if (!ctx.isSuccess) {
        if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
        else end(startIndex, startIndex, count)
      }else {
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        ctx.cut = false
        if (sep == null) rec(beforeSepIndex, nextCount, false)
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
  def repX[V](min: Int)
             (implicit repeater: Implicits.Repeater[T, V],
              ctx: Parse[Any]): Parse[V] = {

    val acc = repeater.initial
    def end(successIndex: Int, index: Int, count: Int) = {
      if (count < min) ctx.prepareFailure(index)
      else ctx.prepareSuccess(repeater.result(acc), successIndex)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
      ctx.cut = precut
      parse0()
      if (!ctx.isSuccess) {
        if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
        else end(startIndex, startIndex, count)
      }else {
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        ctx.cut = false
        rec(beforeSepIndex, nextCount, false)
      }
    }
    ctx.isSuccess = true
    rec(ctx.index, 0, false)
  }
  def rep[V](implicit repeater: Implicits.Repeater[T, V],
             whitespace: Parse[_] => Parse[Unit],
             ctx: Parse[Any]): Parse[V] = {


    val acc = repeater.initial
    def end(successIndex: Int, index: Int, count: Int) = {
      ctx.prepareSuccess(repeater.result(acc), successIndex)
    }
    @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parse[V] = {
      ctx.cut = precut
      parse0()
      if (!ctx.isSuccess){
        if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
        else end(startIndex, startIndex, count)
      }else{
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        if (whitespace ne NoWhitespace.noWhitespaceImplicit) whitespace(ctx)
        ctx.cut = false
        rec(beforeSepIndex, nextCount, false)
      }
    }

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
        parse0()
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
            if (sep == null) rec(beforeSepIndex, nextCount, false)
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
      parse0()
      if (!ctx.isSuccess){
        if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
        else end(startIndex, startIndex, count)
      }else{
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        if (whitespace ne NoWhitespace.noWhitespaceImplicit) whitespace(ctx)
        ctx.cut = false
        if (sep == null) rec(beforeSepIndex, nextCount, false)
        else if (ctx.isSuccess) {
          val sepCut = ctx.cut
          if (whitespace ne NoWhitespace.noWhitespaceImplicit) whitespace(ctx)
          rec(beforeSepIndex, nextCount, sepCut)
        }
        else if (ctx.cut) ctx.prepareFailure(beforeSepIndex)
        else end(beforeSepIndex, beforeSepIndex, nextCount)
      }
    }
    rec(ctx.index, 0, false)
  }
  def rep[V](min: Int)
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
      parse0()
      if (!ctx.isSuccess){
        if (ctx.cut | precut) ctx.asInstanceOf[Parse[V]]
        else end(startIndex, startIndex, count)
      }else{
        val beforeSepIndex = ctx.index
        repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
        val nextCount = count + 1
        if (whitespace ne NoWhitespace.noWhitespaceImplicit) whitespace(ctx)
        ctx.cut = false
        rec(beforeSepIndex, nextCount, false)
      }
    }
    rec(ctx.index, 0, false)
  }

}