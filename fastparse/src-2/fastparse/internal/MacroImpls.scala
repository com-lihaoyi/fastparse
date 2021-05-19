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
        val startIndex = ctx1.index
        lhs.splice
        val res =
          if (!ctx1.isSuccess) ctx1.asInstanceOf[ParsingRun[T]]
          else if (f.splice(ctx1.successValue.asInstanceOf[T])) ctx1.asInstanceOf[ParsingRun[T]]
          else ctx1.freshFailure().asInstanceOf[ParsingRun[T]]

        if (ctx1.verboseFailures) ctx1.aggregateTerminal(startIndex, () => "filter")
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
          if (ctx0.verboseFailures) {
            ctx0.aggregateMsg(
              startIndex,
              Msgs(List(new Lazy(() => name.splice.value))),
              ctx0.failureGroupAggregate,
              startIndex < ctx0.traceIndex
            )
            if (!ctx0.isSuccess){
              ctx0.failureStack = (name.splice.value -> startIndex) :: ctx0.failureStack
            }
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
              if (ctx1.verboseFailures) ctx1.aggregateTerminal(index, () => literalized.splice)
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
                ctx1.aggregateTerminal(index, () => literalized.splice)
              }
              res

            }
          }
        }
      case _ =>
        reify{
          val s1 = s.splice
          ctx.splice match{ case ctx1 =>
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

        lhs0.splice
        val lhsMsg = ctx5.shortParserMsg
        val lhsAggregate = ctx5.failureGroupAggregate
        if (ctx5.isSuccess) {
          ctx5.cut |= oldCut
          ctx5.asInstanceOf[ParsingRun[V]]
        }
        else if (ctx5.cut) ctx5.asInstanceOf[ParsingRun[V]]
        else {
          val verboseFailures = ctx5.verboseFailures

          ctx5.index = startPos
          if (verboseFailures) ctx5.aggregateMsg(startPos, lhsMsg, lhsAggregate)

          ctx5.cut = false
          other.splice
          val rhsMsg = ctx5.shortParserMsg
          val rhsCut = ctx5.cut
          val endCut = rhsCut | oldCut
          if (!ctx5.isSuccess && !rhsCut) ctx5.freshFailure(startPos)
          ctx5.cut = endCut
          if (verboseFailures) ctx5.aggregateMsg(startPos, rhsMsg ::: lhsMsg, ctx5.failureGroupAggregate ::: lhsAggregate)
          ctx5.asInstanceOf[ParsingRun[V]]
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
        if ($ctx1.verboseFailures) $ctx1.setMsg($index, () => $bracketed)
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
        if (ctx1.verboseFailures) ctx1.aggregateTerminal(index, () => bracketed.splice)
        res
      }
    }
  }

  def parsedSequence0[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                     (c: Context)
                     (rhs: c.Expr[ParsingRun[V]], cut: Boolean)
                     (s: c.Expr[Implicits.Sequencer[T, V, R]],
                      whitespace: Option[c.Expr[ParsingRun[Any] => ParsingRun[Unit]]],
                      ctx: c.Expr[ParsingRun[_]]): c.Expr[ParsingRun[R]] = {
    import c.universe._

    val lhs = c.prefix.asInstanceOf[Expr[EagerOps[T]]]
    val ctx1 = TermName(c.freshName("ctx1"))
    val preLhsIndex = TermName(c.freshName("preLhsIndex"))
    val postLhsIndex = TermName(c.freshName("postLhsIndex"))
    val preRhsIndex = TermName(c.freshName("preRhsIndex"))
    val postRhsIndex = TermName(c.freshName("postRhsIndex"))
    val lhsAggregate = TermName(c.freshName("lhsAggregate"))
    val rhsAggregate = TermName(c.freshName("rhsAggregate"))
    val rhsMsg = TermName(c.freshName("rhsMsg"))
    val lhsMsg = TermName(c.freshName("lhsMsg"))
    val rhsMadeProgress = TermName(c.freshName("rhsMadeProgress"))
    val input = TermName(c.freshName("input"))
    val lhsValue = TermName(c.freshName("lhsValue"))
    val nextIndex = TermName(c.freshName("nextIndex"))
    val res = TermName(c.freshName("res"))
    val s1 = TermName(c.freshName("s1"))

    val setCut = c.Expr[Boolean](if(cut) q"$ctx1.cut = true" else q"()")
    val rhsSnippet = q"""
      if (!$ctx1.isSuccess && $ctx1.cut) $ctx1
      else {
        val $preRhsIndex = $ctx1.index
        $rhs
        val $rhsAggregate = $ctx1.failureGroupAggregate
        val $rhsMsg = $ctx1.shortParserMsg
        val $res =
          if (!$ctx1.isSuccess) {
            $setCut
            $ctx1
          } else {
            val $postRhsIndex = $ctx1.index

            val $rhsMadeProgress = $postRhsIndex > $preRhsIndex
            val $nextIndex =
              if (!$rhsMadeProgress && $input.isReachable($postRhsIndex)) $postLhsIndex
              else $postRhsIndex

            if ($rhsMadeProgress && $ctx1.checkForDrop()) $input.dropBuffer($postRhsIndex)

            $ctx1.freshSuccess(
              $s1.apply($lhsValue.asInstanceOf[${c.weakTypeOf[T]}], $ctx1.successValue.asInstanceOf[${c.weakTypeOf[V]}]),
              $nextIndex
            )
          }

        if ($ctx1.verboseFailures) $ctx1.aggregateMsg(
          $preLhsIndex,
          _root_.fastparse.internal.Util.joinBinOp($lhsMsg, $rhsMsg),
          $rhsAggregate ::: $lhsAggregate,
          // We override the failureGroupAggregate to avoid building an `a ~ b`
          // aggregate msg in the specific case where the LHS parser fails to
          // make any progress past `startIndex`. This finds cases like `a.? ~ b`
          // or `a.rep ~ b` and lets use flatten them out into `a | b`
          forceAggregate = $preRhsIndex == $ctx1.traceIndex
        )
        $res
      }
    """
    val guardedRhs = whitespace match{
      case None => rhsSnippet
      case Some(ws) =>
        if (ws.tree.tpe =:= typeOf[fastparse.NoWhitespace.noWhitespaceImplicit.type]) rhsSnippet
        else{
          q"""
            _root_.fastparse.internal.Util.consumeWhitespace($ws, $ctx1)
            if ($ctx1.isSuccess) $rhsSnippet
            else $ctx1
          """
        }
    }
    c.Expr[ParsingRun[R]](q"""{
      $ctx match{ case $ctx1 =>
        $s match{case $s1 =>
          val $preLhsIndex = $ctx1.index
          val $input = $ctx1.input
          $lhs.parse0
          if (!$ctx1.isSuccess) $ctx1
          else {
            val $postLhsIndex = $ctx1.index
            val $lhsAggregate = $ctx1.failureGroupAggregate
            val $lhsMsg = $ctx1.shortParserMsg
            $setCut

            if ($postLhsIndex > $preLhsIndex && $ctx1.checkForDrop()) $input.dropBuffer($postLhsIndex)

            val $lhsValue = $ctx1.successValue
            $guardedRhs
          }
        }
      }
    }.asInstanceOf[_root_.fastparse.ParsingRun[${c.weakTypeOf[R]}]]""")
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

          ctx1.freshSuccess(ctx1.successValue, cut = ctx1.cut | progress).asInstanceOf[ParsingRun[T]]
        }
      }
    }
  }

  def charPredMacro(c: Context)
                   (p: c.Expr[Char => Boolean])
                   (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    import c.universe._
    reify{
      ctx.splice match{case ctx0 =>
        p.splice match{ case p0 =>
          val startIndex = ctx0.index
          val res =
            if (!(ctx0.input.isReachable(ctx0.index) && p0(ctx0.input(ctx0.index)))) {
              ctx0.freshFailure().asInstanceOf[ParsingRun[Unit]]
            }
            else {
              ctx0.freshSuccessUnit(ctx0.index + 1)
            }
          if (ctx0.verboseFailures) ctx0.aggregateTerminal(startIndex, () => s"char-pred(${p0})")
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
    val goal = TermName(c.freshName("goal"))
    val res = q"""
      $ctx match{ case $ctx1 =>
        var $index = $ctx1.index
        val $input = $ctx1.input
        val $start = $index
        val $goal = $min + $start
        while(
          $input.isReachable($index) &&
          ${parseCharCls(c)(c.Expr[Char](q"$input($index)"), Seq(literal))}
        ) $index += 1
        val res =
          if ($index >= $goal) $ctx1.freshSuccessUnit(index = $index)
          else $ctx1.freshFailure()

        if ($ctx1.verboseFailures) $ctx1.aggregateTerminal($start, () => $bracketed)
        res
      }
    """
    c.Expr[ParsingRun[Unit]](res)
  }
  def charsWhileMacro1(c: Context)
                      (p: c.Expr[Char => Boolean])
                      (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    import c.universe._
    charsWhileMacro(c)(p, reify(1))(ctx)
  }
  def charsWhileMacro(c: Context)
                     (p: c.Expr[Char => Boolean], min: c.Expr[Int])
                     (ctx: c.Expr[ParsingRun[Any]]): c.Expr[ParsingRun[Unit]] = {
    import c.universe._

    reify {
      ctx.splice match { case ctx0 =>
        p.splice match{ case p0 =>
          var index = ctx0.index
          val input = ctx0.input
          val start = index
          val goal = min.splice + start
          while (input.isReachable(index) && p0(input(index))) index += 1
          val res =
            if (index >= goal) ctx0.freshSuccessUnit(index = index)
            else ctx0.freshFailure()
          if (ctx0.verboseFailures) ctx0.aggregateTerminal(start, () => s"chars-while($p0, ${min.splice})")
          res
        }
      }
    }
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
          ctx1.cut = false
          lhs0.splice
          val postSuccess = ctx1.isSuccess

          val res =
            if (postSuccess) {
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
            val msg = ctx1.shortParserMsg
            val agg = ctx1.failureGroupAggregate
            if (!postSuccess){
              ctx1.aggregateMsg(startPos, () => msg.render + ".?", agg)
            }
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
