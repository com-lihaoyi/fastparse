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

    def getInlineExpansionValue(t: Term): Option[Int] = {
      t match{
        case Inlined(a, b, c) => getInlineExpansionValue(c)
        case _ => t.asExprOf[Int].value
      }
    }

    val staticMin0 = getInlineExpansionValue(min.asTerm)
    val staticMax0 = getInlineExpansionValue(max.asTerm)
    val staticExactly0 = getInlineExpansionValue(exactly.asTerm)

    val staticActualMin = staticMin0.zip(staticExactly0).map{(m, e) => if (e == -1) m else e}
    val staticActualMax = staticMax0.zip(staticExactly0).map{(m, e) => if (e == -1) m else e}

    '{
      val ctx = $ctx0
      val repeater = $repeater0
      val acc = repeater.initial
      val actualMin = if ($exactly == -1) $min else $exactly
      val actualMax = if ($exactly == -1) $max else $exactly

      def end(successIndex: Int, index: Int, count: Int, endCut: Boolean) = ${
        staticActualMin match{
          case Some(-1) => '{ ctx.freshSuccess(repeater.result(acc), successIndex, endCut) }
          case _ =>
            '{
              if (count < actualMin) ctx.augmentFailure(index, endCut)
              else ctx.freshSuccess(repeater.result(acc), successIndex, endCut)
            }
        }
      }

      @tailrec def rec(startIndex: Int,
                       count: Int,
                       precut: Boolean,
                       outerCut: Boolean,
                       sepMsg: Msgs,
                       lastAgg: Msgs): ParsingRun[V] = ${

        def consumeWhitespace(extraCut: Expr[Boolean])(x: Expr[ParsingRun[V]]) =
          if whitespace.asTerm.tpe =:= TypeRepr.of[fastparse.NoWhitespace.noWhitespaceImplicit.type]
          then x
          else '{
            Util.consumeWhitespace($whitespace, ctx)
            if (!ctx.isSuccess && ($extraCut || ctx.cut)) ctx.asInstanceOf[ParsingRun[Nothing]]
            else { $x }
          }

        val ctxCut = staticActualMin match{
          case Some(-1) => '{ precut }
          case _ => '{ precut | (count < actualMin && outerCut) }
        }

        val checkMax0 = staticActualMax match{
          case Some(v) if v != 0 => '{false}
          case _ => '{ count == 0 && actualMax == 0 }
        }

        '{
          ctx.cut = $ctxCut
          if ($checkMax0) ctx.freshSuccess(repeater.result(acc), startIndex)
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
              ${
                val checkMax2 = staticActualMax match {
                  case Some(Int.MaxValue) => '{ false }
                  case _ => '{ nextCount == actualMax }
                }
                '{
                  if ($checkMax2) {
                    val res = end(beforeSepIndex, beforeSepIndex, nextCount, outerCut | postCut)
                    if (verboseFailures) ctx.setMsg(startIndex, () => parsedMsg.render + ".rep" + (if (actualMin == 0) "" else s"(${actualMin})"))
                    res
                  }
                  else {
                    ${
                      consumeWhitespace('{false})('{
                        ctx.cut = false
                        ${
                          sep match {
                            case '{ null } =>
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
                                  ${
                                    consumeWhitespace('{sepCut})('{
                                      rec(beforeSepIndex, nextCount, sepCut, endCut, ctx.shortParserMsg, parsedAgg)
                                    })
                                  }
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
                      })
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
