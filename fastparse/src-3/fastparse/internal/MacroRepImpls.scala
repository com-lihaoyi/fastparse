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
  def repXMacro0[T: Type, V: Type](
      lhs: Expr[ParsingRun[T]],
      whitespace: Null | Expr[fastparse.Whitespace],
      min: Null | Expr[Int],
  )(repeater: Expr[Implicits.Repeater[T, V]], ctx: Expr[ParsingRun[_]])(using quotes: Quotes): Expr[ParsingRun[V]] = {
    import quotes.reflect.*

    '{

      val ctx1        = $ctx
      val repeater1   = $repeater
      var originalCut = ctx1.cut
      val acc         = repeater1.initial
      @ _root_.scala.annotation.tailrec
      def rec(
          startIndex: _root_.scala.Int,
          count: _root_.scala.Int,
          lastAgg: _root_.fastparse.internal.Msgs
      ): _root_.fastparse.P[V] = {
        ${
          val (endSnippet, aggregateSnippet, minCut) = min match {
            case null =>
              (
                '{
                  ctx1.freshSuccess(repeater1.result(acc), startIndex, originalCut)
                },
                '{ "" },
                '{ false }
              )
            case min1 =>
              (
                '{
                  if (count < $min1) ctx1.augmentFailure(startIndex, originalCut)
                  else ctx1.freshSuccess(repeater1.result(acc), startIndex, originalCut)
                },
                '{ if ($min1 == 0) "" else "(" + $min1 + ")" },
                '{ originalCut && (count < $min1) }
              )
          }

          '{

            ctx1.cut = $minCut
            $lhs

            val parsedMsg = ctx1.shortParserMsg
            val parsedAgg = ctx1.failureGroupAggregate
            originalCut |= ctx1.cut
            if (!ctx1.isSuccess) {
              val res =
                if (ctx1.cut) ctx1.asInstanceOf[_root_.fastparse.P[V]]
                else $endSnippet
              if (ctx1.verboseFailures) {
                ctx1.aggregateMsg(
                  startIndex,
                  () => parsedMsg.render + s".rep" + $aggregateSnippet,
                  if (lastAgg == null) ctx1.failureGroupAggregate
                  else ctx1.failureGroupAggregate ::: lastAgg
                )
              }
              res
            } else {
              val beforeSepIndex = ctx1.index
              repeater1.accumulate(ctx1.successValue.asInstanceOf[T], acc)
              ctx1.cut = false
              ${
                val wsSnippet = whitespace match {
                  case null => '{ rec(beforeSepIndex, count + 1, parsedAgg) }
                  case ws =>
                    '{
                      if ($ws ne _root_.fastparse.NoWhitespace.noWhitespaceImplicit) {
                        _root_.fastparse.internal.Util.consumeWhitespace($ws, ctx1)
                      }
                      if (!ctx1.isSuccess && ctx1.cut) ctx1.asInstanceOf[_root_.fastparse.ParsingRun[scala.Nothing]]
                      else {
                        ctx1.cut = false
                        rec(beforeSepIndex, count + 1, parsedAgg)
                      }
                    }
                }
                wsSnippet
              }
            }
          }
        }
      }
      rec(ctx1.index, 0, null)
    }
  }
}
