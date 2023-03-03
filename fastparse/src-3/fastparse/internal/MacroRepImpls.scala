package fastparse.internal

import fastparse.{Implicits, NoWhitespace, P, ParsingRun}

import scala.annotation.tailrec
import scala.quoted.*

/** Implementations of the various `.rep`/`.repX` overloads. The most common
  * and simple overloads are implemented as macros for performance, while the
  * more complex/general cases are left as normal methods to avoid code bloat
  * and allow the use of default/named arguments (which don't work in macros
  * due to https://github.com/scala/bug/issues/5920).
  *
  * Even the normal method overloads are manually-specialized to some extent
  * for various sorts of inputs as a best-effort attempt ot minimize branching
  * in the hot paths.
  */
object MacroRepImpls {
  def repMacro0[T: Type, V: Type](
      parse0: Expr[ParsingRun[T]],
      sep: Expr[ParsingRun[_]],
      whitespace: Expr[fastparse.Whitespace],
      min: Expr[Int],
      max: Expr[Int],
      exactly: Expr[Int],
  )(repeater0: Expr[Implicits.Repeater[T, V]],
    ctx0: Expr[ParsingRun[_]])(using quotes: Quotes): Expr[ParsingRun[V]] = {
    import quotes.reflect.*

    '{


      val ctx = $ctx0
      val repeater = $repeater0
      val acc = repeater.initial
      val actualMin = if ($exactly == -1) $min else $exactly
      val actualMax = if ($exactly == -1) $max else $exactly

      def end(successIndex: Int, index: Int, count: Int, endCut: Boolean) = {
        if (count < actualMin) ctx.augmentFailure(index, endCut)
        else ctx.freshSuccess(repeater.result(acc), successIndex, endCut)
      }

      @tailrec def rec(startIndex: Int,
                       count: Int,
                       precut: Boolean,
                       outerCut: Boolean,
                       sepMsg: Msgs,
                       lastAgg: Msgs): ParsingRun[V] = ${

        val consumeWhitespace =
          if whitespace.asTerm.tpe =:= TypeRepr.of[fastparse.NoWhitespace.noWhitespaceImplicit.type]
          then '{ Util.consumeWhitespace($whitespace, ctx) }
          else '{}

        '{
          ctx.cut = precut | (count < actualMin && outerCut)
          if (count == 0 && actualMax == 0) ctx.freshSuccess(repeater.result(acc), startIndex)
          else {
            $parse0
            val parsedMsg = ctx.shortParserMsg
            val parsedAgg = ctx.failureGroupAggregate
            val postCut = ctx.cut
            val verboseFailures = ctx.verboseFailures
            if (!ctx.isSuccess) {
              val res =
                if (postCut) ctx.asInstanceOf[ParsingRun[V]]
                else end(startIndex, startIndex, count, outerCut | postCut)
              if (verboseFailures) Util.aggregateMsgInRep(startIndex, actualMin, ctx, sepMsg, parsedMsg, lastAgg, precut)
              res
            } else {
              val beforeSepIndex = ctx.index
              repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
              val nextCount = count + 1
              if (nextCount == actualMax) {
                val res = end(beforeSepIndex, beforeSepIndex, nextCount, outerCut | postCut)
                if (verboseFailures) ctx.setMsg(startIndex, () => parsedMsg.render + ".rep" + (if (actualMin == 0) "" else s"(${actualMin})"))
                res
              }
              else {
                $consumeWhitespace

                if (!ctx.isSuccess && ctx.cut) ctx.asInstanceOf[ParsingRun[Nothing]]
                else {
                  ctx.cut = false
                  ${
                    sep match {
                      case '{ null } if false =>
                        '{
                          rec(beforeSepIndex, nextCount, false, outerCut | postCut, null, parsedAgg)
                        }
                      case _ =>
                        '{
                          val sep1 = $sep
                          val sepCut = ctx.cut
                          val endCut = outerCut | postCut | sepCut
                          if (sep1 == null) rec(beforeSepIndex, nextCount, false, endCut, null, parsedAgg)
                          else if (ctx.isSuccess) {
                            $consumeWhitespace
                            if (!ctx.isSuccess && sepCut) ctx.asInstanceOf[ParsingRun[Nothing]]
                            else rec(beforeSepIndex, nextCount, sepCut, endCut, ctx.shortParserMsg, parsedAgg)
                          }
                          else {
                            val res =
                              if (sepCut) ctx.augmentFailure(beforeSepIndex, endCut)
                              else end(beforeSepIndex, beforeSepIndex, nextCount, endCut)

                            if (verboseFailures) Util.aggregateMsgPostSep(startIndex, actualMin, ctx, parsedMsg, parsedAgg)
                            res
                          }
                        }
                    }
                  }

                }
              }
            }
          }
        }
      }

      rec(ctx.index, 0, false, ctx.cut, null, null)
    }
  }
}
