package fastparse.internal

import fastparse.{Implicits, ParserInput, ParsingRun}

import scala.quoted.*

object MacroInlineImpls {

  def literalStrMacro(s: Expr[String])(ctx: Expr[ParsingRun[Any]])(using quotes: Quotes): Expr[ParsingRun[Unit]] = {
    import quotes.reflect.*

    s.value match {
      case Some(x) =>
        val literalized = Expr[String](Util.literalize(x))
        if (x.length == 0) '{ $ctx.freshSuccessUnit() }
        else if (x.length == 1) {
          val charLiteral = Expr[Char](x.charAt(0))
          '{

            $ctx match {
              case ctx1 =>
                val input = ctx1.input
                val index = ctx1.index
                val res =
                  if (input.isReachable(index) && input(index) == $charLiteral) {
                    ctx1.freshSuccessUnit(index + 1)
                  } else {
                    ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
                  }
                if (ctx1.verboseFailures) ctx1.aggregateTerminal(index, () => $literalized)
                res
            }

          }
        } else {
          val xLength = Expr[Int](x.length)
          '{
            $ctx match {
              case ctx1 =>
                val index = ctx1.index
                val end   = index + $xLength
                val input = ctx1.input
                val res =
                  if (input.isReachable(end - 1) && ${
                    x.zipWithIndex
                      .map { case (char, i) => '{ input.apply(index + ${ Expr(i) }) == ${ Expr(char) } } }
                      .reduce[Expr[Boolean]] { case (l, r) => '{ $l && $r } }
                  }) {
                    ctx1.freshSuccessUnit(end)
                  } else {
                    ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
                  }
                if (ctx1.verboseFailures) {
                  ctx1.aggregateTerminal(index, () => $literalized)
                }
                res

            }
          }
        }
      case None =>
        '{
          val s1 = $s
          $ctx match {
            case ctx1 =>
              val index = ctx1.index
              val res =
                if (Util.startsWith(ctx1.input, s1, index)) ctx1.freshSuccessUnit(index + s1.length)
                else ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
              if (ctx1.verboseFailures) ctx1.aggregateTerminal(index, () => Util.literalize(s1))
              res
          }
        }
    }

  }

  inline def filterInline[T](inline lhs: ParsingRun[_])(f: T => Boolean)(ctx1: ParsingRun[_]): ParsingRun[T] = {
    val startIndex = ctx1.index
    lhs
    val res: ParsingRun[T] =
      if (!ctx1.isSuccess) ctx1.asInstanceOf[ParsingRun[T]]
      else if (f(ctx1.successValue.asInstanceOf[T])) ctx1.asInstanceOf[ParsingRun[T]]
      else ctx1.freshFailure().asInstanceOf[ParsingRun[T]]

    if (ctx1.verboseFailures) ctx1.aggregateTerminal(startIndex, () => "filter")
    res
  }

  inline def pInline[T](inline t: ParsingRun[T])(
      name: sourcecode.Name,
      ctx1: ParsingRun[_]
  ): ParsingRun[T] = {

    val startIndex = ctx1.index
    val instrument = ctx1.instrument != null
    if (instrument) {
      ctx1.instrument.beforeParse(name.value, startIndex)
    }
    val ctx0       = t

    if (instrument) {
      ctx1.instrument.afterParse(name.value, ctx0.index, ctx0.isSuccess)
    }
    if (ctx0.verboseFailures) {
      ctx0.aggregateMsg(
        startIndex,
        Msgs(new Lazy(() => name.value) :: Nil),
        ctx0.failureGroupAggregate,
        startIndex < ctx0.traceIndex
      )
      if (!ctx0.isSuccess) {
        ctx0.failureStack = (name.value -> startIndex) :: ctx0.failureStack
      }
    }
    ctx0
  }

  inline def cutInline[T](inline lhs: ParsingRun[_])(ctx0: ParsingRun[_]): ParsingRun[T] = {
    val startIndex = ctx0.index
    val ctx1       = lhs
    val index      = ctx1.index
    if (!ctx1.isSuccess) ctx1.augmentFailure(index)
    else {
      val progress = index > startIndex
      if (progress && ctx1.checkForDrop()) ctx1.input.dropBuffer(index)

      ctx1.freshSuccess(ctx1.successValue, cut = ctx1.cut | progress).asInstanceOf[ParsingRun[T]]
    }
  }

  def parsedSequence0[T: Type, V: Type, R: Type](lhs: Expr[ParsingRun[T]], rhs: Expr[ParsingRun[V]], cut: Boolean)(
      s: Expr[Implicits.Sequencer[T, V, R]],
      whitespace: Null | Expr[fastparse.Whitespace],
      ctx: Expr[ParsingRun[_]]
  )(using quotes: Quotes): Expr[ParsingRun[R]] = {
    import quotes.reflect.*

    def setCut(ctx1: Expr[ParsingRun[Any]]): Expr[Unit] = if cut then '{ $ctx1.cut = true }
    else '{}

    '{
      {
        val ctx1        = $ctx
        val s1          = $s
        val preLhsIndex = ctx1.index
        val input       = ctx1.input
        $lhs
        if (!ctx1.isSuccess) ctx1
        else {
          val postLhsIndex = ctx1.index
          val lhsAggregate = ctx1.failureGroupAggregate
          val lhsMsg       = ctx1.shortParserMsg
          ${ setCut('{ ctx1 }) }

          if (postLhsIndex > preLhsIndex && ctx1.checkForDrop()) input.dropBuffer(postLhsIndex)

          val lhsValue = ctx1.successValue
          ${

            val rhsSnippet = '{
              if (!ctx1.isSuccess && ctx1.cut) ctx1
              else {
                val preRhsIndex = ctx1.index
                $rhs
                val rhsAggregate = ctx1.failureGroupAggregate
                val rhsMsg       = ctx1.shortParserMsg
                val res =
                  if (!ctx1.isSuccess) {
                    ${ setCut('{ ctx1 }) }
                    ctx1
                  } else {
                    val postRhsIndex = ctx1.index

                    val rhsMadeProgress = postRhsIndex > preRhsIndex
                    val nextIndex =
                      if (!rhsMadeProgress && input.isReachable(postRhsIndex)) postLhsIndex
                      else postRhsIndex

                    if (rhsMadeProgress && ctx1.checkForDrop()) input.dropBuffer(postRhsIndex)

                    ctx1.freshSuccess(
                      s1.apply(
                        lhsValue.asInstanceOf[T],
                        ctx1.successValue.asInstanceOf[V]
                      ),
                      nextIndex
                    )
                  }

                if (ctx1.verboseFailures) ctx1.aggregateMsg(
                  preLhsIndex,
                  _root_.fastparse.internal.Util.joinBinOp(lhsMsg, rhsMsg),
                  rhsAggregate ::: lhsAggregate,
                  // We override the failureGroupAggregate to avoid building an `a ~ b`
                  // aggregate msg in the specific case where the LHS parser fails to
                  // make any progress past `startIndex`. This finds cases like `a.? ~ b`
                  // or `a.rep ~ b` and lets use flatten them out into `a | b`
                  forceAggregate = preRhsIndex == ctx1.traceIndex
                )
                res
              }
            }

            val guardedRhs = whitespace match {
              case null => rhsSnippet
              case ws =>
                if (ws.asTerm.tpe =:= TypeRepr.of[fastparse.NoWhitespace.noWhitespaceImplicit.type]) rhsSnippet
                else {
                  '{
                    _root_.fastparse.internal.Util.consumeWhitespace($ws, ctx1)
                    if (ctx1.isSuccess) $rhsSnippet
                    else ctx1
                  }
                }
            }
            guardedRhs
          }
        }
      }.asInstanceOf[_root_.fastparse.ParsingRun[R]]
    }
  }

  inline def optionInline[T, V](inline lhs0: ParsingRun[T])(
      optioner1: Implicits.Optioner[T, V],
      ctx1: ParsingRun[Any]
  ): ParsingRun[V] = {
    val startPos = ctx1.index
    val startCut = ctx1.cut
    ctx1.cut = false
    lhs0
    val postSuccess = ctx1.isSuccess

    val res =
      if (postSuccess) {
        val res = ctx1.freshSuccess(optioner1.some(ctx1.successValue.asInstanceOf[T]))
        res.cut |= startCut
        res
      } else if (ctx1.cut) ctx1.asInstanceOf[ParsingRun[V]]
      else {
        val res = ctx1.freshSuccess(optioner1.none, startPos)
        res.cut |= startCut
        res
      }

    if (ctx1.verboseFailures) {
      val msg = ctx1.shortParserMsg
      val agg = ctx1.failureGroupAggregate
      if (!postSuccess) {
        ctx1.aggregateMsg(startPos, () => msg.render + ".?", agg)
      }
    }
    res
  }

  inline def mapInline[T, V](lhs: ParsingRun[T])(inline f: T => V): ParsingRun[V] = {
    if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
    else {
      val this2 = lhs.asInstanceOf[ParsingRun[V]]
      this2.successValue = f(this2.successValue.asInstanceOf[T])
      this2
    }
  }

  inline def collectInline[T, V](lhs: ParsingRun[T])(inline f: PartialFunction[T, V]): ParsingRun[V] = {
    if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
    else {
      val this2 = lhs.asInstanceOf[ParsingRun[V]]
      val f2    = f.andThen(v => this2.successValue = v)
      f2.applyOrElse(this2.successValue.asInstanceOf[T], { (_: T) => this2.freshFailure() })
      this2
    }
  }

  inline def flatMapXInline[T, V](lhs: ParsingRun[T])(inline f: T => ParsingRun[V]): ParsingRun[V] = {
    if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
    else f(lhs.successValue.asInstanceOf[T])
  }

  inline def flatMapInline[T, V](
      lhs: ParsingRun[T]
  )(inline f: T => ParsingRun[V])(ws: fastparse.Whitespace): ParsingRun[V] = {
    if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
    else {
      val oldCapturing = lhs.noDropBuffer
      val successValue = lhs.successValue
      lhs.noDropBuffer = true
      ws(lhs)
      lhs.noDropBuffer = oldCapturing
      if (!lhs.isSuccess && lhs.cut) lhs.asInstanceOf[ParsingRun[V]]
      else f(successValue.asInstanceOf[T])
    }
  }

  inline def eitherInline[T, V >: T](inline lhs0: ParsingRun[T])(inline other: ParsingRun[V])(ctx5: ParsingRun[Any])
      : ParsingRun[V] = {

    val oldCut = ctx5.cut
    ctx5.cut = false
    val startPos = ctx5.index

    lhs0
    val lhsMsg       = ctx5.shortParserMsg
    val lhsAggregate = ctx5.failureGroupAggregate
    if (ctx5.isSuccess) {
      ctx5.cut |= oldCut
      ctx5.asInstanceOf[ParsingRun[V]]
    } else if (ctx5.cut) ctx5.asInstanceOf[ParsingRun[V]]
    else {
      val verboseFailures = ctx5.verboseFailures

      ctx5.index = startPos
      if (verboseFailures) ctx5.aggregateMsg(startPos, lhsMsg, lhsAggregate)

      ctx5.cut = false
      other
      val rhsMsg = ctx5.shortParserMsg
      val rhsCut = ctx5.cut
      val endCut = rhsCut | oldCut
      if (!ctx5.isSuccess && !rhsCut) ctx5.freshFailure(startPos)
      ctx5.cut = endCut
      if (verboseFailures)
        ctx5.aggregateMsg(startPos, rhsMsg ::: lhsMsg, ctx5.failureGroupAggregate ::: lhsAggregate)
      ctx5.asInstanceOf[ParsingRun[V]]
    }
  }

  inline def captureInline(inline lhs0: ParsingRun[Any])(ctx6: ParsingRun[Any]): ParsingRun[String] = {
    val startPos     = ctx6.index
    val oldCapturing = ctx6.noDropBuffer
    ctx6.noDropBuffer = true
    lhs0
    ctx6.noDropBuffer = oldCapturing

    if (!ctx6.isSuccess) ctx6.asInstanceOf[ParsingRun[String]]
    else ctx6.freshSuccess(ctx6.input.slice(startPos, ctx6.index))
  }

  def parseCharCls(char: Expr[Char], ss: Seq[String])(using quotes: Quotes): Expr[Boolean] = {
    import quotes.reflect.*

    val snippets = for (s <- ss) yield {
      val output = collection.mutable.Buffer.empty[Either[Char, (Char, Char)]]
      var i      = 0
      while (i < s.length) {
        s(i) match {
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
        output.collect { case Left(char) => CaseDef(Expr(char).asTerm, None, Expr(true).asTerm) },
        output.collect { case Right((l, h)) => (l, h) }
      )
    }

    val (literals, ranges) = snippets.unzip

    '{
      val charIn = $char
      ${
        Match(
          '{ charIn }.asTerm,
          literals.flatten.toList :+ CaseDef(
            Wildcard(),
            None,
            ranges.flatten.map { (l, h) => '{ ${ Expr(l) } <= charIn && charIn <= ${ Expr(h) } } }.reduceOption {
              (l, r) => '{ $l || $r }
            }.getOrElse(Expr(false)).asTerm
          )
        ).asExprOf[Boolean]
      }
    }
  }

  def charInMacro(s: Expr[Seq[String]])(ctx: Expr[ParsingRun[Any]])(using quotes: Quotes): Expr[ParsingRun[Unit]] = {
    import quotes.reflect.*

    val literals: Seq[String] = getLiteralStrings(s)

    val parsed    = parseCharCls('{ $ctx.input($ctx.index) }, literals)
    val bracketed = Expr[String](literals.map(l => "[" + Util.literalize(l).drop(1).dropRight(1) + "]").mkString)
    '{
      $ctx match {
        case ctx1 =>
          val index = ctx1.index
          val res =
            if (!ctx1.input.isReachable(index)) {
              ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
            } else $parsed match {
              case true  => ctx1.freshSuccessUnit(index + 1)
              case false => ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
            }
          if (ctx1.verboseFailures) ctx1.aggregateTerminal(index, () => $bracketed)
          res
      }
    }
  }

  private def getLiteralStrings(s: Expr[Seq[String]])(using quotes: Quotes): Seq[String] = {
    import quotes.reflect.*
    s match {
      case Varargs(args @ Exprs(argValues)) => argValues
      case _ =>
        report.errorAndAbort("Function can only accept constant singleton type", s)
    }
  }

  inline def charPredInline(inline p0: Char => Boolean)(ctx0: ParsingRun[Any]): ParsingRun[Unit] = {
    val startIndex = ctx0.index
    val res =
      if (!(ctx0.input.isReachable(ctx0.index) && p0(ctx0.input(ctx0.index)))) {
        ctx0.freshFailure().asInstanceOf[ParsingRun[Unit]]
      } else {
        ctx0.freshSuccessUnit(ctx0.index + 1)
      }
    if (ctx0.verboseFailures) ctx0.aggregateTerminal(startIndex, () => s"char-pred(${p0})")
    res
  }

  def charsWhileInMacro(s: Expr[String], min: Expr[Int])(ctx: Expr[ParsingRun[Any]])(using
      quotes: Quotes
  ): Expr[ParsingRun[Unit]] = {
    import quotes.reflect.*

    val literal = s.value.getOrElse(report.errorAndAbort("Function can only accept constant singleton type", s))

    val bracketed = Expr[String]("[" + Util.literalize(literal).drop(1).dropRight(1) + "]")

    '{
      $ctx match {
        case ctx1 =>
          var index = ctx1.index
          val input = ctx1.input
          val start = index
          val goal  = $min + start
          while (
            input.isReachable(index) &&
            ${ parseCharCls('{ input(index) }, Seq(literal)) }
          ) index += 1
          val res =
            if (index >= goal) ctx1.freshSuccessUnit(index = index)
            else ctx1.freshFailure()

          if (ctx1.verboseFailures) ctx1.aggregateTerminal(start, () => $bracketed)
          res
      }
    }
  }

  inline def charsWhileInline(inline p0: Char => Boolean, min: Int)(ctx0: ParsingRun[Any]): ParsingRun[Unit] = {
    var index = ctx0.index
    val input = ctx0.input
    val start = index
    val goal  = min + start
    while (input.isReachable(index) && p0(input(index))) index += 1
    val res =
      if (index >= goal) ctx0.freshSuccessUnit(index = index)
      else ctx0.freshFailure()
    if (ctx0.verboseFailures) ctx0.aggregateTerminal(start, () => s"chars-while($p0, $min)")
    res
  }

  def stringInMacro0(
      ignoreCaseExpr: Expr[Boolean],
      s: Expr[Seq[String]]
  )(ctx: Expr[ParsingRun[Any]])(using quotes: Quotes): Expr[ParsingRun[Unit]] = {

    import quotes.reflect.*

    val ignoreCase = ignoreCaseExpr.valueOrAbort

    val literals = getLiteralStrings(s)
    val trie = new CompactTrieNode(
      new TrieNode(if (ignoreCase) literals.map(_.toLowerCase) else literals)
    )

    '{
      $ctx match {
        case ctx1 =>
          val index = ctx1.index
          val input = ctx1.input

          var output: Int       = -1
          def setOutput(x: Int) = output = x

          ${

            def charAtN(n: Expr[Int]): Expr[Char] =
              if (ignoreCase) '{ input.apply($n).toLower }
              else '{ input.apply($n) }
            def rec(depth: Int, t: CompactTrieNode): Expr[Unit] = '{
              val n = index + ${ Expr(depth) }
              ${
                if t.word
                then '{ setOutput(n) }
                else '{}
              }
              if (input.isReachable(n)) ${
                if (t.children.isEmpty) '{ () }
                else {
                  val casedefs = t.children.map {
                    case (k, ("", v)) => CaseDef(Expr(k).asTerm, None, rec(depth + 1, v).asTerm)
                    case (k, (s, v)) =>
                      val checks = s
                        .zipWithIndex
                        .map { case (char, i) =>
                          '{ ${ charAtN('{ index + ${ Expr(depth + i + 1) } }) } == ${ Expr(char) } }
                        }
                        .reduce[Expr[Boolean]] { case (l, r) => '{ $l && $r } }
                      CaseDef(
                        Expr(k).asTerm,
                        None,
                        '{
                          if (input.isReachable(index + ${ Expr(depth + s.length) }) && $checks) {
                            ${ rec(depth + s.length + 1, v) }
                          }
                        }.asTerm
                      )
                  }

                  Match(charAtN('{ n }).asTerm, casedefs.toList :+ CaseDef(Wildcard(), None, '{}.asTerm))
                    .asExprOf[Unit]
                }
              }
            }

            rec(0, trie)

          }

          val res =
            if (output != -1) ctx1.freshSuccessUnit(output)
            else ctx1.freshFailure()
          if (ctx1.verboseFailures) ctx1.setMsg(
            index,
            () =>
              ${
                Expr("StringIn(" + literals.map(Util.literalize(_)).mkString(", ") + ")")
              }
          )
          res
      }
    }
  }

}
