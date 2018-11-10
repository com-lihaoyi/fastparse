package fastparse.internal

import fastparse.{EagerOps, Implicits, ParserInput, ParsingRun}

import scala.annotation.tailrec
import scala.reflect.macros.blackbox.Context

/**
  * Fastparse relies heavily on macro-based inlining to provide good
  * performance: inlining allows for better optimization at each callsite
  * compared to all callsites calling into the same shared function body.
  * Most of these macros do not do anything fancy and simply inline the body
  * of a plain-old-function, although a handful do some partial evaluation
  * e.g. optimizing the `LiteralStr` and `CharIn` macros in cases where the
  * String/Char values are known at compile time.
  */
object MacroImpls {
  def filterMacro[T: c.WeakTypeTag](c: Context)
                                   (f: c.Expr[T => Boolean])
                                   (ctx: c.Expr[ParsingRun[_]]) = {
    import c.universe._
    val lhs = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify{
      ctx.splice match{ case ctx1 =>
        lhs.splice
        val res =
          if (!ctx1.isSuccess) ctx1.asInstanceOf[ParsingRun[T]]
          else if (f.splice(ctx1.successValue.asInstanceOf[T])) ctx1.asInstanceOf[ParsingRun[T]]
          else ctx1.freshFailure().asInstanceOf[ParsingRun[T]]

        if (ctx1.verboseFailures) ctx1.aggregateTerminal(() => "filter")
        res
      }
    }
  }
  def pMacro[T: c.WeakTypeTag](c: Context)
                              (t: c.Expr[ParsingRun[T]])
                              (name: c.Expr[sourcecode.Name],
                               ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[T]] = {

    import c.universe._
    reify[ParsingRun[T]]{
      ctx.splice match{case ctx1 =>
        val startIndex = ctx1.index
        val instrument = ctx1.instrument != null
        if (instrument) {
          ctx1.instrument.beforeParse(name.splice.value, startIndex)
        }
        t.splice match{case ctx0 =>
          if (instrument) {
            ctx1.instrument.afterParse(name.splice.value, ctx0.index, ctx0.isSuccess)
          }
          if (ctx0.verboseFailures && !ctx0.isSuccess) {
            ctx0.setMsg(() => name.splice.value)
            ctx0.failureStack = (name.splice.value -> startIndex) :: ctx0.failureStack
          }
          ctx0
        }
      }
    }
  }

  def literalStrMacro(c: Context)
                     (s: c.Expr[String])
                     (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    import c.universe._
    s.actualType match{
      case ConstantType(Constant(x: String)) =>
        val literalized = c.Expr[String](Literal(Constant(Util.literalize(x))))
        if (x.length == 0) reify{ ctx.splice.freshSuccessUnit() }
        else if (x.length == 1){
          val charLiteral = c.Expr[Char](Literal(Constant(x.charAt(0))))
          reify {

            ctx.splice match{ case ctx1 =>
              val input = ctx1.input
              val index = ctx1.index
              val res =
                if (input.isReachable(index) && input(index) == charLiteral.splice){
                  ctx1.freshSuccessUnit(index + 1)
                }else{
                  ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
                }
              if (ctx1.verboseFailures) ctx1.aggregateTerminal(() => literalized.splice)
              res
            }

          }
        }else{
          val xLength = c.Expr[Int](Literal(Constant(x.length)))
          val checker = c.Expr[(ParserInput, Int) => Boolean]{
            val stringSym = TermName(c.freshName("string"))
            val offsetSym = TermName(c.freshName("offset"))
            val checks = x
              .zipWithIndex
              .map { case (char, i) => q"""$stringSym.apply($offsetSym + $i) == $char""" }
              .reduce[Tree]{case (l, r) => q"$l && $r"}

            q"($stringSym: _root_.fastparse.ParserInput, $offsetSym: _root_.scala.Int) => $checks"
          }
          reify {

            ctx.splice match{ case ctx1 =>
              val index = ctx1.index
              val end = index + xLength.splice
              val input = ctx1.input
              val res =
                if (input.isReachable(end - 1)  && checker.splice(input, index) ) {
                  ctx1.freshSuccessUnit(end)
                }else {
                  ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
                }
              if (ctx1.verboseFailures) {
                ctx1.aggregateTerminal(() => literalized.splice)
              }
              res

            }
          }
        }
      case _ =>
        reify{
          val s1 = s.splice
          ctx.splice match{ case ctx1 =>
            val res =
              if (Util.startsWith(ctx1.input, s1, ctx1.index)) ctx1.freshSuccessUnit(ctx1.index + s1.length)
              else ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
            if (ctx1.verboseFailures) ctx1.aggregateTerminal(() => Util.literalize(s1))
            res
          }
        }
    }

  }

  def mapMacro[T: c.WeakTypeTag, V: c.WeakTypeTag]
              (c: Context)
              (f: c.Expr[T => V]): c.Expr[ParsingRun[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      lhs0.splice.parse0 match{ case lhs =>
        if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
        else {
          val this2 = lhs.asInstanceOf[ParsingRun[V]]
          this2.successValue = f.splice(this2.successValue.asInstanceOf[T])
          this2
        }
      }
    }
  }


  def flatMapXMacro[T: c.WeakTypeTag, V: c.WeakTypeTag]
                  (c: Context)
                  (f: c.Expr[T => ParsingRun[V]]): c.Expr[ParsingRun[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      val lhs = lhs0.splice.parse0
      if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
      else f.splice(lhs.successValue.asInstanceOf[T])
    }
  }

  def flatMapMacro[T: c.WeakTypeTag, V: c.WeakTypeTag]
                  (c: Context)
                  (f: c.Expr[T => ParsingRun[V]])
                  (whitespace: c.Expr[ParsingRun[Any] => ParsingRun[Unit]]): c.Expr[ParsingRun[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      val lhs = lhs0.splice.parse0
      whitespace.splice match{ case ws =>
        if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
        else {
          val oldCapturing = lhs.noDropBuffer
          val successValue = lhs.successValue
          lhs.noDropBuffer = true
          ws(lhs)
          lhs.noDropBuffer = oldCapturing
          if (!lhs.isSuccess && lhs.cut) lhs.asInstanceOf[ParsingRun[V]]
          else f.splice(successValue.asInstanceOf[T])
        }
      }
    }
  }

  def eitherMacro[T: c.WeakTypeTag, V >: T: c.WeakTypeTag]
                  (c: Context)
                  (other: c.Expr[ParsingRun[V]])
                  (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      ctx.splice match { case ctx5 =>
        val oldCut = ctx5.cut
        ctx5.cut = false
        val startPos = ctx5.index
        val startGroup = ctx5.failureGroupAggregate
        lhs0.splice
        val lhsMsg = ctx5.shortParserMsg
        if (ctx5.isSuccess) {
          ctx5.cut |= oldCut
          ctx5.asInstanceOf[ParsingRun[V]]
        }
        else if (ctx5.cut) ctx5.asInstanceOf[ParsingRun[V]]
        else {
          if (ctx5.verboseFailures) ctx5.aggregateMsg(lhsMsg, startGroup)

          ctx5.index = startPos
          ctx5.setMsg(ctx5.shortParserMsg)
          ctx5.cut = false
          other.splice
          val rhsMsg = ctx5.shortParserMsg
          val res =
            if (ctx5.isSuccess) {
              ctx5.cut |= oldCut
              ctx5
            }
            else if (ctx5.cut) ctx5
            else {
              val res = ctx5.freshFailure(startPos)
              ctx5.cut |= oldCut
              res
            }
          if (ctx5.verboseFailures) ctx5.setMsg(rhsMsg ::: lhsMsg)
          res.asInstanceOf[ParsingRun[V]]
        }
      }

    }
  }

  def captureMacro(c: Context)
                  (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[String]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[_]]]

    reify {
      ctx.splice match{ case ctx6 =>
        val startPos = ctx6.index
        val oldCapturing = ctx6.noDropBuffer
        ctx6.noDropBuffer = true
        lhs0.splice
        ctx6.noDropBuffer = oldCapturing

        if (!ctx6.isSuccess) ctx6.asInstanceOf[ParsingRun[String]]
        else ctx6.freshSuccess(ctx6.input.slice(startPos, ctx6.index))
      }
    }
  }


  def eagerOpsStrMacro(c: Context)
                      (parse0: c.Expr[String])
                      (ctx: c.Expr[ParsingRun[Any]]): c.Expr[fastparse.EagerOps[Unit]] = {
    import c.universe._
    val literal = literalStrMacro(c)(parse0)(ctx)
    reify{ fastparse.EagerOps[Unit](literal.splice)}
  }


  def stringInMacro(c: Context)
                   (s: c.Expr[String]*)
                   (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    stringInMacro0(c)(false, s:_*)(ctx)
  }
  def stringInIgnoreCaseMacro(c: Context)
                             (s: c.Expr[String]*)
                             (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    stringInMacro0(c)(true, s:_*)(ctx)
  }
  def stringInMacro0(c: Context)
                    (ignoreCase: Boolean, s: c.Expr[String]*)
                    (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    import c.universe._

    val literals = s.map(_.actualType match{
      case ConstantType(Constant(x: String)) => x
      case _ => c.abort(c.enclosingPosition, "Function can only accept constant singleton type")
    })
    val trie = new CompactTrieNode(
      new TrieNode(if (ignoreCase) literals.map(_.toLowerCase) else literals)
    )
    val ctx1 = TermName(c.freshName("ctx"))
    val output = TermName(c.freshName("output"))
    val index = TermName(c.freshName("index"))
    val input = TermName(c.freshName("input"))
    val n = TermName(c.freshName("n"))
    def charAtN(n: Tree) = if (ignoreCase) q"$input.apply($n).toLower" else q"$input.apply($n)"
    def rec(depth: Int, t: CompactTrieNode): c.Expr[Unit] = {
      val charAt = charAtN(q"$n")
      val children = if (t.children.isEmpty) q"()"
      else {
        q"""
        $charAt match {
          case ..${t.children.map {
            case (k, ("", v)) => cq"$k => ${rec(depth + 1, v)}"
            case (k, (s, v)) =>
              val checks = s
                .zipWithIndex
                .map { case (char, i) => q"""${charAtN(q"$index + ${depth + i + 1}")} == $char""" }
                .reduce[Tree]{case (l, r) => q"$l && $r"}
              cq"""
                $k => if ($input.isReachable($index + ${depth + s.length}) && $checks){
                  ${rec(depth + s.length + 1, v)}
                }
              """
          }}
          case _ =>
        }
        """
      }

      c.Expr[Unit](
        q"""
        val $n = $index + $depth
        ..${if (t.word) Seq(q"$output = $n") else Nil}
        if ($input.isReachable($n)) $children

        """
      )
    }

    val bracketed = "StringIn(" + literals.map(Util.literalize(_)).mkString(", ") + ")"

    val res = q"""
      $ctx match{ case $ctx1 =>

        val $index = $ctx1.index
        val $input = $ctx1.input

        var $output: Int = -1
        ${rec(0, trie)}
        val res =
          if ($output != -1) $ctx1.freshSuccessUnit(index = $output)
          else $ctx1.freshFailure()
        if ($ctx1.verboseFailures) $ctx1.setMsg(() => $bracketed)
        res
      }
    """

    c.Expr[ParsingRun[Unit]](res)

  }

  def parseCharCls(c: Context)(char: c.Expr[Char], ss: Seq[String]) = {
    import c.universe._

    val snippets = for(s <- ss) yield{
      val output = collection.mutable.Buffer.empty[Either[Char, (Char, Char)]]
      var i = 0
      while(i < s.length){
        s(i) match{
          case '\\' =>
            i += 1
            output.append(Left(s(i)))
          case '-' =>
            i += 1
            val Left(last) = output.remove(output.length - 1)
            output.append(Right((last, s(i))))
          case c => output.append(Left(c))
        }
        i += 1
      }

      (
        output.collect{case Left(char) => cq"$char => true"},
        output.collect{case Right((l, h)) => q"$l <= charIn && charIn <= $h"}
      )
    }

    val (literals, ranges) = snippets.unzip
    c.Expr[Boolean](q"""$char match{
      case ..${literals.flatten}
      case charIn => ${ranges.flatten.reduceOption{ (l, r) => q"$l || $r"}.getOrElse(q"false")}
    }""")
  }

  def charInMacro(c: Context)
                 (s: c.Expr[String]*)
                 (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    import c.universe._

    val literals = s.map(_.actualType match{
      case ConstantType(Constant(x: String)) => x
      case _ => c.abort(c.enclosingPosition, "Function can only accept constant singleton type")
    })

    val parsed = parseCharCls(c)(reify(ctx.splice.input(ctx.splice.index)), literals)
    val bracketed = c.Expr[String](
      Literal(Constant(literals.map(l => "[" + Util.literalize(l).drop(1).dropRight(1) + "]").mkString))
    )
    reify {
      ctx.splice match { case ctx1 =>
        val index = ctx1.index
        val res =
          if (!ctx1.input.isReachable(index)) {
            ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
          } else parsed.splice match {
            case true => ctx1.freshSuccessUnit(index + 1)
            case false => ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
          }
        if (ctx1.verboseFailures) ctx1.aggregateTerminal(() => bracketed.splice)
        res
      }
    }
  }

  def parsedSequence0[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                     (c: Context)
                     (other: c.Expr[ParsingRun[V]], cut: Boolean)
                     (s: c.Expr[Implicits.Sequencer[T, V, R]],
                      whitespace: Option[c.Expr[ParsingRun[Any] => ParsingRun[Unit]]],
                      ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    import c.universe._

    val lhs = c.prefix.asInstanceOf[Expr[EagerOps[T]]]
    val cut1 = c.Expr[Boolean](if(cut) q"true" else q"false")
    val consumeWhitespace = whitespace match{
      case None => reify{(c: ParsingRun[Any]) => true}
      case Some(ws) =>
        if (ws.tree.tpe =:= typeOf[fastparse.NoWhitespace.noWhitespaceImplicit.type]){
          reify{(c: ParsingRun[Any]) => true}
        }else{
          reify{(c: ParsingRun[Any]) =>
            val oldCapturing = c.noDropBuffer // completely disallow dropBuffer
            c.noDropBuffer = true
            ws.splice(c)
            c.noDropBuffer = oldCapturing
            c.isSuccess
          }
        }
    }

    reify {
      {
        ctx.splice match{ case ctx1 =>
          s.splice match{case s1 =>
            val startIndex = ctx1.index
            val input = ctx1.input
            lhs.splice.parse0
            if (!ctx1.isSuccess) ctx1
            else {
              val lhsMsg = ctx1.shortParserMsg
              ctx1.cut |= cut1.splice
              val index = ctx1.index
              if (index > startIndex && ctx1.checkForDrop()) input.dropBuffer(index)

              val pValue = ctx1.successValue
              val preWsIndex = index
              if (!consumeWhitespace.splice(ctx1)) ctx1
              else {
                if (!ctx1.isSuccess && ctx1.cut) ctx1
                else {
                  val preOtherIndex = ctx1.index
                  other.splice
                  val postOtherIndex = ctx1.index

                  val rhsNewCut = cut1.splice | ctx1.cut
                  val msg = ctx1.shortParserMsg
                  val res =
                    if (!ctx1.isSuccess) ctx1.augmentFailure(
                      postOtherIndex,
                      cut = rhsNewCut
                    ) else {
                      val rhsMadeProgress = postOtherIndex > preOtherIndex
                      val nextIndex =
                        if (!rhsMadeProgress && input.isReachable(postOtherIndex)) preWsIndex
                        else postOtherIndex

                      if (rhsMadeProgress && ctx1.checkForDrop()) input.dropBuffer(postOtherIndex)

                      ctx1.freshSuccess(
                        s1.apply(pValue.asInstanceOf[T], ctx1.successValue.asInstanceOf[V]),
                        nextIndex
                      )
                    }
                  if (ctx1.verboseFailures) ctx1.setMsg(() => Util.parenthize(lhsMsg) + " ~ " + Util.parenthize(msg))
                  res
                }
              }
            }
          }
        }
      }.asInstanceOf[ParsingRun[R]]
    }
  }

  def cutMacro[T: c.WeakTypeTag](c: Context)(ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[T]] = {
    import c.universe._
    val lhs = c.prefix.asInstanceOf[c.Expr[EagerOps[_]]]
    reify{
      ctx.splice match { case ctx0 =>
        val startIndex = ctx0.index
        val ctx1 = lhs.splice.parse0
        val index = ctx1.index
        if (!ctx1.isSuccess) ctx1.augmentFailure(index)
        else {
          val progress = index > startIndex
          if (progress && ctx1.checkForDrop()) ctx1.input.dropBuffer(index)
          val msg = ctx1.shortParserMsg

          val res =
            ctx1.freshSuccess(
              ctx1.successValue,
              cut = ctx1.cut | progress
            ).asInstanceOf[ParsingRun[T]]
          if (ctx1.verboseFailures) ctx1.setMsg(() => Util.parenthize(msg) + "./")
          res
        }
      }
    }
  }


  def charsWhileInMacro1(c: Context)
                        (s: c.Expr[String])
                        (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    import c.universe._
    charsWhileInMacro(c)(s, reify(1))(ctx)
  }
  def charsWhileInMacro(c: Context)
                       (s: c.Expr[String], min: c.Expr[Int])
                       (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    import c.universe._

    val literal = s.actualType match{
      case ConstantType(Constant(x: String)) => x
      case _ => c.abort(c.enclosingPosition, "Function can only accept constant singleton type")
    }

    val bracketed = c.Expr[String](Literal(Constant("[" + Util.literalize(literal).drop(1).dropRight(1) + "]")))

    val ctx1 = TermName(c.freshName("ctx"))
    val index = TermName(c.freshName("index"))
    val input = TermName(c.freshName("input"))
    val start = TermName(c.freshName("start"))
    val res = q"""
      $ctx match{ case $ctx1 =>
        var $index = $ctx1.index
        val $input = $ctx1.input

        val $start = $index
        while(
          $input.isReachable($index) &&
          ${parseCharCls(c)(c.Expr[Char](q"$input($index)"), Seq(literal))}
        ) $index += 1
        val res =
          if ($index - $start >= $min) $ctx1.freshSuccessUnit(index = $index)
          else {
            $ctx1.isSuccess = false
            $ctx1.asInstanceOf[fastparse.P[Unit]]
          }
        if ($ctx1.verboseFailures) $ctx1.aggregateTerminal(() => $bracketed)
        res
      }
    """
    c.Expr[ParsingRun[Unit]](res)
  }


  def byNameOpsStrMacro(c: Context)
                       (parse0: c.Expr[String])
                       (ctx: c.Expr[ParsingRun[Any]]): c.Expr[fastparse.ByNameOps[Unit]] = {
    import c.universe._
    val literal = MacroImpls.literalStrMacro(c)(parse0)(ctx)
    reify{ new fastparse.ByNameOps[Unit](() => literal.splice)}
  }

  def logOpsStrMacro(c: Context)
                       (parse0: c.Expr[String])
                       (ctx: c.Expr[ParsingRun[Any]]): c.Expr[fastparse.LogByNameOps[Unit]] = {
    import c.universe._
    val literal = MacroImpls.literalStrMacro(c)(parse0)(ctx)
    reify{ new fastparse.LogByNameOps[Unit](literal.splice)(ctx.splice)}
  }

  def optionMacro[T: c.WeakTypeTag, V: c.WeakTypeTag](c: Context)
                 (optioner: c.Expr[Implicits.Optioner[T, V]],
                  ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[V]] = {
    import c.universe._
    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[_]]]
    reify{
      ctx.splice match{ case ctx1 =>
        optioner.splice match { case optioner1 =>
          val startPos = ctx1.index
          val startCut = ctx1.cut
          val startGroup = ctx1.failureGroupAggregate
          ctx1.cut = false
          lhs0.splice
          val msg = ctx1.shortParserMsg
          val res =
            if (ctx1.isSuccess) {
              val res = ctx1.freshSuccess(optioner1.some(ctx1.successValue.asInstanceOf[T]))
              res.cut |= startCut
              res
            }
            else if (ctx1.cut) ctx1.asInstanceOf[ParsingRun[V]]
            else {
              val res = ctx1.freshSuccess(optioner1.none, startPos)
              res.cut |= startCut
              res
            }

          if (ctx1.verboseFailures) {
            ctx1.aggregateMsg(List(() => Util.parenthize(msg) + ".?"), msg, startGroup)
          }
          res
        }
      }
    }
  }

  def parsedSequence[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[ParsingRun[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]],
                     whitespace: c.Expr[ParsingRun[Any] => ParsingRun[Unit]],
                     ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    MacroImpls.parsedSequence0[T, V, R](c)(other, false)(s, Some(whitespace), ctx)
  }

  def parsedSequenceCut[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                       (c: Context)
                       (other: c.Expr[ParsingRun[V]])
                       (s: c.Expr[Implicits.Sequencer[T, V, R]],
                        whitespace: c.Expr[ParsingRun[Any] => ParsingRun[Unit]],
                        ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    MacroImpls.parsedSequence0[T, V, R](c)(other, true)(s, Some(whitespace), ctx)
  }
  def parsedSequence1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                     (c: Context)
                     (other: c.Expr[ParsingRun[V]])
                     (s: c.Expr[Implicits.Sequencer[T, V, R]],
                      ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    MacroImpls.parsedSequence0[T, V, R](c)(other, false)(s, None, ctx)
  }
  def parsedSequenceCut1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                        (c: Context)
                        (other: c.Expr[ParsingRun[V]])
                        (s: c.Expr[Implicits.Sequencer[T, V, R]],
                         ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    MacroImpls.parsedSequence0[T, V, R](c)(other, true)(s, None, ctx)
  }

}
