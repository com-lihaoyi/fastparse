package fastparse.internal

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

        @fastparse.NoWarn.nowarn
        val isNotNoWhitespace = $ws ne _root_.fastparse.NoWhitespace.noWhitespaceImplicit
        if (isNotNoWhitespace) {
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

            val $parsedMsg = $ctx1.shortMsg
            val $parsedAgg = $ctx1.aggregateMsgs
            $originalCut |= $ctx1.cut
            if (!$ctx1.isSuccess) {
              val res =
                if ($ctx1.cut) $ctx1.asInstanceOf[_root_.fastparse.P[${c.weakTypeOf[V]}]]
                else $endSnippet

              if ($ctx1.verboseFailures) _root_.fastparse.internal.Util.reportParseMsgInRep(
                $startIndex,
                ${min.getOrElse(q"0")},
                $ctx1,
                _root_.fastparse.internal.Msgs.empty,
                $parsedMsg,
                $lastAgg,
                true
              )

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
