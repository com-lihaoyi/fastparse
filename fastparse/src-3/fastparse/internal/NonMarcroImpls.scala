package fastparse.internal

import fastparse.{EagerOps, Implicits, ParserInput, ParsingRun}

import scala.quoted.*

object NonMarcroImpls {

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
          val checker =
            '{ (string: _root_.fastparse.ParserInput, offset: _root_.scala.Int) =>
              ${
                x.zipWithIndex
                  .map { case (char, i) => '{ string.apply(offset + ${ Expr(i) }) == ${ Expr(char) } } }
                  .reduce[Expr[Boolean]] { case (l, r) => '{ $l && $r } }
              }
            }
          '{

            $ctx match {
              case ctx1 =>
                val index = ctx1.index
                val end   = index + $xLength
                val input = ctx1.input
                val res =
                  if (input.isReachable(end - 1) && ${ checker }(input, index)) {
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

  inline def filterNonMacro[T](inline lhs: ParsingRun[_])(f: T => Boolean)(ctx1: ParsingRun[_]): ParsingRun[T] = {
    val startIndex = ctx1.index
    lhs
    val res: ParsingRun[T] =
      if (!ctx1.isSuccess) ctx1.asInstanceOf[ParsingRun[T]]
      else if (f(ctx1.successValue.asInstanceOf[T])) ctx1.asInstanceOf[ParsingRun[T]]
      else ctx1.freshFailure().asInstanceOf[ParsingRun[T]]

    if (ctx1.verboseFailures) ctx1.aggregateTerminal(startIndex, () => "filter")
    res
  }

  def pNonMacro[T](t: () => ParsingRun[T])(
      name: sourcecode.Name,
      ctx1: ParsingRun[_]
  ): ParsingRun[T] = {

    val startIndex = ctx1.index
    val instrument = ctx1.instrument != null
    val ctx0       = t()
    if (instrument) {
      ctx1.instrument.beforeParse(name.value, startIndex)
    }
    if (instrument) {
      ctx1.instrument.afterParse(name.value, ctx0.index, ctx0.isSuccess)
    }
    if (ctx0.verboseFailures) {
      ctx0.aggregateMsg(
        startIndex,
        Msgs(List(new Lazy(() => name.value))),
        ctx0.failureGroupAggregate,
        startIndex < ctx0.traceIndex
      )
      if (!ctx0.isSuccess) {
        ctx0.failureStack = (name.value -> startIndex) :: ctx0.failureStack
      }
    }
    ctx0
  }

  def cutNonMacro[T](lhs: () => ParsingRun[_])(ctx0: ParsingRun[_]): ParsingRun[T] = {
    val startIndex = ctx0.index
    val ctx1       = lhs()
    val index      = ctx1.index
    if (!ctx1.isSuccess) ctx1.augmentFailure(index)
    else {
      val progress = index > startIndex
      if (progress && ctx1.checkForDrop()) ctx1.input.dropBuffer(index)

      ctx1.freshSuccess(ctx1.successValue, cut = ctx1.cut | progress).asInstanceOf[ParsingRun[T]]
    }
  }

  def literalStrNonMacro(s1: String)(ctx1: ParsingRun[Any]): ParsingRun[Unit] = {
    val index = ctx1.index
    val res =
      if (Util.startsWith(ctx1.input, s1, index)) ctx1.freshSuccessUnit(index + s1.length)
      else ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
    if (ctx1.verboseFailures) ctx1.aggregateTerminal(index, () => Util.literalize(s1))
    res
  }

  def parsedSequence0[T, V, R](lhs: () => ParsingRun[_])(
      rhs: () => ParsingRun[V],
      cut: Boolean
  )(
      s: Implicits.Sequencer[T, V, R],
      whitespace: Option[ParsingRun[Any] => ParsingRun[Unit]],
      ctx: ParsingRun[_]
  ): ParsingRun[R] = {

    def setCut(ctx1: ParsingRun[_]): Unit = if (cut) ctx1.cut = true else ()
    def rhsSnippet(
        ctx1: ParsingRun[_],
        s1: Implicits.Sequencer[T, V, R],
        lhsValue: T,
        preLhsIndex: Int,
        lhsAggregate: Msgs,
        postLhsIndex: Int,
        input: ParserInput,
        lhsMsg: Msgs
    ) = {
      if (!ctx1.isSuccess && ctx1.cut) ctx1
      else {
        val preRhsIndex = ctx1.index
        rhs()
        val rhsAggregate = ctx1.failureGroupAggregate
        val rhsMsg       = ctx1.shortParserMsg
        val res =
          if (!ctx1.isSuccess) {
            setCut(ctx1)
            ctx1
          } else {
            val postRhsIndex = ctx1.index

            val rhsMadeProgress = postRhsIndex > preRhsIndex
            val nextIndex =
              if (!rhsMadeProgress && input.isReachable(postRhsIndex)) postLhsIndex
              else postRhsIndex

            if (rhsMadeProgress && ctx1.checkForDrop()) input.dropBuffer(postRhsIndex)

            ctx1.freshSuccess(
              s1.apply(lhsValue, ctx1.successValue.asInstanceOf[V]),
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

    def guardedRhs(
        ctx1: ParsingRun[_],
        s1: Implicits.Sequencer[T, V, R],
        lhsValue: T,
        preLhsIndex: Int,
        lhsAggregate: Msgs,
        postLhsIndex: Int,
        input: ParserInput,
        lhsMsg: Msgs
    ) = whitespace match {
      case None => rhsSnippet(ctx1, s1, lhsValue, preLhsIndex, lhsAggregate, postLhsIndex, input, lhsMsg)
      case Some(ws) =>
        if (ws.isInstanceOf[fastparse.NoWhitespace.noWhitespaceImplicit.type])
          rhsSnippet(ctx1, s1, lhsValue, preLhsIndex, lhsAggregate, postLhsIndex, input, lhsMsg)
        else {
          fastparse.internal.Util.consumeWhitespace(ws, ctx1)
          if (ctx1.isSuccess) rhsSnippet(ctx1, s1, lhsValue, preLhsIndex, lhsAggregate, postLhsIndex, input, lhsMsg)
          else ctx1
        }
    }

    {
      val ctx1        = ctx
      val s1          = s
      val preLhsIndex = ctx1.index
      val input       = ctx1.input
      lhs()
      if (!ctx1.isSuccess) ctx1
      else {
        val postLhsIndex = ctx1.index
        val lhsAggregate = ctx1.failureGroupAggregate
        val lhsMsg       = ctx1.shortParserMsg
        setCut(ctx1)

        if (postLhsIndex > preLhsIndex && ctx1.checkForDrop()) input.dropBuffer(postLhsIndex)

        val lhsValue = ctx1.successValue
        guardedRhs(ctx1, s1, lhsValue.asInstanceOf[T], preLhsIndex, lhsAggregate, postLhsIndex, input, lhsMsg)
      }
    }.asInstanceOf[_root_.fastparse.ParsingRun[R]]
  }

  def optionNonMacro[T, V](lhs0: () => ParsingRun[T])(
      optioner1: Implicits.Optioner[T, V],
      ctx1: ParsingRun[Any]
  ): ParsingRun[V] = {
    val startPos = ctx1.index
    val startCut = ctx1.cut
    ctx1.cut = false
    lhs0()
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

  def mapNonMacro[T, V](lhs: ParsingRun[T])(f: T => V): ParsingRun[V] = {
    if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
    else {
      val this2 = lhs.asInstanceOf[ParsingRun[V]]
      this2.successValue = f(this2.successValue.asInstanceOf[T])
      this2
    }
  }

  def collectNonMacro[T, V](lhs: ParsingRun[T])(f: PartialFunction[T, V]): ParsingRun[V] = {
    if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
    else {
      val this2 = lhs.asInstanceOf[ParsingRun[V]]
      val f2    = f.andThen(v => this2.successValue = v)
      f2.applyOrElse(this2.successValue.asInstanceOf[T], { (_: T) => this2.freshFailure() })
      this2
    }
  }

  def flatMapXNonMacro[T, V](lhs: ParsingRun[T])(f: T => ParsingRun[V]): ParsingRun[V] = {
    if (!lhs.isSuccess) lhs.asInstanceOf[ParsingRun[V]]
    else f(lhs.successValue.asInstanceOf[T])
  }

  def flatMapNonMacro[T, V](
      lhs: ParsingRun[T]
  )(f: T => ParsingRun[V])(ws: ParsingRun[Any] => ParsingRun[Unit]): ParsingRun[V] = {
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

  def eitherNonMacro[T, V >: T](lhs0: () => ParsingRun[T])(other: () =>ParsingRun[V])(ctx5: ParsingRun[Any])
      : ParsingRun[V] = {

    val oldCut = ctx5.cut
    ctx5.cut = false
    val startPos = ctx5.index

    lhs0()
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
      other()
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

  def captureNonMacro(lhs0: () => ParsingRun[Any])(ctx6: ParsingRun[Any]): ParsingRun[String] = {
    val startPos     = ctx6.index
    val oldCapturing = ctx6.noDropBuffer
    ctx6.noDropBuffer = true
    lhs0()
    ctx6.noDropBuffer = oldCapturing

    if (!ctx6.isSuccess) ctx6.asInstanceOf[ParsingRun[String]]
    else ctx6.freshSuccess(ctx6.input.slice(startPos, ctx6.index))
  }

  def parseCharCls(ss: Seq[String]): Char => Boolean = {

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
        output.collect { case Left(char) => char },
        output.collect { case Right((l, h)) => Range.inclusive(l, h) }
      )
    }

    val (literals, ranges) = snippets.unzip
    val literalSet         = literals.flatten.toSet
    val flatRange          = ranges.flatten.toList
    (char: Char) => {
      literalSet.contains(char) || flatRange.exists(_.contains(char))
    }
  }

  def charInNonMacro(literals: String*)(ctx1: ParsingRun[Any]): ParsingRun[Unit] = {

    val charPred  = parseCharCls(literals)
    def char      = ctx1.input(ctx1.index)
    val bracketed = literals.map(l => "[" + Util.literalize(l).drop(1).dropRight(1) + "]").mkString
    val index     = ctx1.index
    val res =
      if (!ctx1.input.isReachable(index)) {
        ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
      } else if (charPred(char)) {
        ctx1.freshSuccessUnit(index + 1)
      } else {
        ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
      }
    if (ctx1.verboseFailures) ctx1.aggregateTerminal(index, () => bracketed)
    res
  }

  def charPredNonMacro(p0: Char => Boolean)(ctx0: ParsingRun[Any]): ParsingRun[Unit] = {
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

  def charsWhileInMacro(literal: String, min: Int)(ctx1: ParsingRun[Any]): ParsingRun[Unit] = {

    val bracketed = "[" + Util.literalize(literal).drop(1).dropRight(1) + "]"

    var index   = ctx1.index
    val input   = ctx1.input
    val start   = index
    val goal    = min + start
    val matcher = parseCharCls(Seq(literal))
    while (
      input.isReachable(index) &&
      matcher(input(index))
    ) index += 1
    val res =
      if (index >= goal) ctx1.freshSuccessUnit(index = index)
      else ctx1.freshFailure()

    if (ctx1.verboseFailures) ctx1.aggregateTerminal(start, () => bracketed)
    res
  }

  def charsWhileMacro(p0: Char => Boolean, min: Int)(ctx0: ParsingRun[Any]): ParsingRun[Unit] = {
    var index = ctx0.index
    val input = ctx0.input
    val start = index
    val goal  = min + start
    while (input.isReachable(index) && p0(input(index))) index += 1
    val res =
      if (index >= goal) ctx0.freshSuccessUnit(index = index)
      else ctx0.freshFailure()
    if (ctx0.verboseFailures) ctx0.aggregateTerminal(start, () => s"chars-while($p0, $min")
    res
  }

  def stringInMacro0(ignoreCase: Boolean, literals: String*)(ctx1: ParsingRun[Any]): ParsingRun[Unit] = {

    val trie = new CompactTrieNode(
      new TrieNode(if (ignoreCase) literals.map(_.toLowerCase) else literals)
    )

    val $ctx1  = ctx1
    def $index = $ctx1.index
    val $input = $ctx1.input

    var $output: Int = -1

    def charAtN(n: Int) = if (ignoreCase) $input.apply(n).toLower else $input.apply(n)

    def rec(depth: Int, t: CompactTrieNode): Unit = {
      def $n = $index + depth

      if (t.word) $output = $n
      if ($input.isReachable($n)) {
        if (t.children.isEmpty) ()
        else {
          val $charAt = charAtN($n)
          t.children.collectFirst {
            case (`$charAt`, tup) => tup
          } match {
            case Some(("", v)) => rec(depth + 1, v)
            case Some((s, v)) =>
              def $checks = s
                .zipWithIndex
                .forall { case (char, i) => charAtN($index + depth + i + 1) == char }

              if ($input.isReachable($index + depth + s.length) && $checks) {
                rec(depth + s.length + 1, v)
              }
            case _ =>
          }
        }
      }
    }

    val $bracketed = "StringIn(" + literals.map(Util.literalize(_)).mkString(", ") + ")"

    rec(0, trie)
    val res =
      if ($output != -1) $ctx1.freshSuccessUnit(index = $output)
      else $ctx1.freshFailure()
    if ($ctx1.verboseFailures) $ctx1.setMsg($index, () => $bracketed)
    res

  }

}
