package fasterparser

import fasterparser.Parsing.EagerOps

import scala.reflect.macros.blackbox.Context

object MacroImpls {

  def pMacro[T: c.WeakTypeTag](c: Context)
                              (t: c.Expr[Parse[T]])
                              (name: c.Expr[sourcecode.Name]): c.Expr[Parse[T]] = {

    import c.universe._
    val ctx = c.Expr[Parse[_]](q"implicitly[fasterparser.Parse[_]]")
    reify[Parse[T]]{
      val startIndex = ctx.splice.index
      t.splice match{case ctx0 =>
        if ((ctx0.traceIndex != -1 | ctx0.logDepth != 0) && !ctx0.isSuccess) {
          ctx0.failureStack = (name.splice.value -> startIndex) :: ctx0.failureStack
        }
        ctx0
      }
    }
  }
  def literalStrMacro(c: Context)(s: c.Expr[String])(ctx: c.Expr[Parse[Any]]): c.Expr[Parse[Unit]] = {
    import c.universe._
    s.actualType match{
      case ConstantType(Constant(x: String)) =>
        val literalized = c.Expr[String](Literal(Constant(Util.literalize(x))))
        if (x.length == 0) reify{ ctx.splice.freshSuccess((), "") }
        else if (x.length == 1){
          val charLiteral = c.Expr[Char](Literal(Constant(x.charAt(0))))
          reify {
            ctx.splice match{ case ctx1 =>
              if (ctx1.index < ctx1.input.length && ctx1.input(ctx1.index) == charLiteral.splice){
                ctx1.freshSuccess((), literalized.splice, ctx1.index + 1)
              }else{
                ctx1.freshFailure(literalized.splice).asInstanceOf[Parse[Unit]]
              }
            }
          }
        }else{
          val xLength = c.Expr[Int](Literal(Constant(x.length)))
          val checker = c.Expr[(String, Int) => Boolean]{
            val stringSym = TermName(c.freshName("string"))
            val offsetSym = TermName(c.freshName("offset"))
            val checks = x
              .zipWithIndex
              .map { case (char, i) => q"""$stringSym.charAt($offsetSym + $i) == $char""" }
              .reduce[Tree]{case (l, r) => q"$l && $r"}

            q"($stringSym: java.lang.String, $offsetSym: scala.Int) => $checks"
          }
          reify {
            ctx.splice match{ case ctx1 =>
              if (ctx1.index + xLength.splice <= ctx1.input.length &&
                checker.splice(ctx1.input, ctx1.index)){
                ctx1.freshSuccess((), literalized.splice, ctx1.index + xLength.splice)
              }else{
                ctx1.freshFailure(literalized.splice).asInstanceOf[Parse[Unit]]
              }
            }
          }
        }
      case _ =>
        reify{
          val s1 = s.splice
          ctx.splice match{ case ctx1 =>
            if (ctx1.input.startsWith(s1, ctx1.index)) ctx1.freshSuccess((), Util.literalize(s1), ctx1.index + s1.length)
            else ctx1.freshFailure(Util.literalize(s1)).asInstanceOf[Parse[Unit]]
          }
        }
    }

  }

  def mapMacro[T: c.WeakTypeTag, V: c.WeakTypeTag]
              (c: Context)
              (f: c.Expr[T => V]): c.Expr[Parse[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      lhs0.splice.parse0 match{ case lhs =>
        if (!lhs.isSuccess) lhs.asInstanceOf[Parse[V]]
        else {
          val this2 = lhs.asInstanceOf[Parse[V]]
          this2.successValue = f.splice(this2.successValue.asInstanceOf[T])
          this2
        }
      }
    }
  }


  def flatMapMacro[T: c.WeakTypeTag, V: c.WeakTypeTag]
                  (c: Context)
                  (f: c.Expr[T => Parse[V]]): c.Expr[Parse[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      val lhs = lhs0.splice.parse0
      if (!lhs.isSuccess) lhs.asInstanceOf[Parse[V]]
      else {
        val sCut = lhs.cut
        val res = f.splice(lhs.successValue.asInstanceOf[T])
        res.cut |= sCut
        res
      }
    }
  }

  def eitherMacro[T: c.WeakTypeTag, V >: T: c.WeakTypeTag]
                  (c: Context)
                  (other: c.Expr[Parse[V]])
                  (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[V]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[T]]]
    reify {
      val ctx5 = ctx.splice.asInstanceOf[Parse[V]]
      val oldCut = ctx5.cut
      ctx5.cut = false
      val startPos = ctx5.index
      lhs0.splice
      if (ctx5.isSuccess | ctx5.cut) ctx5
      else {
        ctx5.cut = false
        ctx5.index = startPos
        other.splice
        if (ctx5.isSuccess) {
          ctx5.cut = oldCut
          ctx5
        }else if (ctx5.cut) ctx5
        else {
          val res = ctx5.prepareFailure(startPos)
          ctx5.cut = ctx5.cut | oldCut
          ctx5.failureStack = Nil
          if (ctx5.traceIndex == -1) ctx5.shortFailureMsg = () => "???"
          res
        }
      }

    }
  }

  def captureMacro(c: Context)
                  (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[String]] = {
    import c.universe._

    val lhs0 = c.prefix.asInstanceOf[c.Expr[EagerOps[_]]]

    reify {
      val ctx6 = ctx.splice
      val startPos = ctx6.index
      lhs0.splice
      if (!ctx6.isSuccess) ctx6.asInstanceOf[Parse[String]]
      else ctx6.prepareSuccess(ctx6.input.substring(startPos, ctx6.index))
    }
  }


  def eagerOpsStrMacro(c: Context)
                      (parse0: c.Expr[String])
                      (ctx: c.Expr[Parse[Any]]): c.Expr[fasterparser.Parsing.EagerOps[Unit]] = {
    import c.universe._
    val literal = literalStrMacro(c)(parse0)(ctx)
    reify{ fasterparser.Parsing.EagerOps[Unit](literal.splice)}
  }


  def stringInMacro(c: Context)
                   (s: c.Expr[String]*)
                   (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[Unit]] = {
    stringInMacro0(c)(false, s:_*)(ctx)
  }
  def stringInIgnoreCaseMacro(c: Context)
                             (s: c.Expr[String]*)
                             (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[Unit]] = {
    stringInMacro0(c)(true, s:_*)(ctx)
  }
  def stringInMacro0(c: Context)
                    (ignoreCase: Boolean, s: c.Expr[String]*)
                    (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[Unit]] = {
    import c.universe._

    val literals = s.map(_.actualType match{
      case ConstantType(Constant(x: String)) => x
      case _ => c.abort(c.enclosingPosition, "Function can only accept constant singleton type")
    })
    val trie = new TrieNode(if (ignoreCase) literals.map(_.toLowerCase) else literals)
    val ctx1 = TermName(c.freshName("ctx"))
    val output = TermName(c.freshName("output"))
    val index = TermName(c.freshName("index"))
    val input = TermName(c.freshName("input"))
    val inputLength = TermName(c.freshName("inputLength"))
    val n = TermName(c.freshName("n"))
    def rec(depth: Int, t: TrieNode): c.Expr[Unit] = {
      val charAt = if (ignoreCase) q"$input.charAt($n).toLower" else q"$input.charAt($n)"
      val children = if (t.children.size == 0) q"()"
      else if (t.children.size > 1){
        q"""
        $charAt match {
          case ..${t.children.map { case (k, v) => cq"$k => ${rec(depth + 1, v)}" }}
          case _ =>
        }
        """
      }else{
        q"""
        if($charAt == ${t.children.keys.head}) ${rec(depth + 1, t.children.values.head)}
        """
      }
      val run = q"""
         val $n = $index + $depth
         ..${if (t.word) Seq(q"$output = $n") else Nil}
         if ($n < $inputLength) $children
      """
      val wrap = TermName(c.freshName("wrap"))
      c.Expr[Unit](
        // Best effort attempt to break up the huge methods that tend to be
        // created by StringsIn. Not exact, but hopefully will result in
        // multiple smaller methods being created
        if (!t.break) run
        else q"""
          def $wrap() = $run
          $wrap()
        """
      )
    }

    val bracketed = "StringIn(" + literals.map(Util.literalize(_)).mkString(", ") + ")"

    val res = q"""
      val $ctx1 = $ctx
      val $index = $ctx1.index
      val $input = $ctx1.input
      val $inputLength = $input.length

      var $output: Int = -1
      ${rec(0, trie)}
      if ($output != -1) $ctx1.freshSuccess((), $bracketed, index = $output)
      else {
        $ctx1.isSuccess = false
        if ($ctx1.traceIndex == -1){
          $ctx1.shortFailureMsg = () => $bracketed
        }else if ($ctx1.traceIndex == $index){
          $ctx1.aggregateFailure($bracketed)
        }
        $ctx1.asInstanceOf[Parse[Unit]]

      }
    """

    c.Expr[Parse[Unit]](res)

  }
  final class TrieNode(strings: Seq[String], ignoreCase: Boolean = false) {

    val ignoreCaseStrings = if (ignoreCase) strings.map(_.map(_.toLower)) else strings
    val children = ignoreCaseStrings.filter(!_.isEmpty)
      .groupBy(_(0))
      .map { case (k,ss) => k -> new TrieNode(ss.map(_.tail), ignoreCase) }

    val rawSize = children.values.map(_.size).sum + children.size

    val break = rawSize >= 8
    val size: Int = if (break) 1 else rawSize
    val word: Boolean = strings.exists(_.isEmpty) || children.isEmpty
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
                 (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[Unit]] = {
    import c.universe._

    val literals = s.map(_.actualType match{
      case ConstantType(Constant(x: String)) => x
      case _ => c.abort(c.enclosingPosition, "Function can only accept constant singleton type")
    })

    val parsed = parseCharCls(c)(reify(ctx.splice.input(ctx.splice.index)), literals)
    val bracketed = c.Expr[String](Literal(Constant(literals.map("[" + _ + "]").mkString)))
    reify {
      if (!(ctx.splice.index < ctx.splice.input.length)) {
        ctx.splice.freshFailure(bracketed.splice).asInstanceOf[Parse[Unit]]
      } else parsed.splice match {
        case true => ctx.splice.freshSuccess((), bracketed.splice, ctx.splice.index + 1)
        case false => ctx.splice.freshFailure(bracketed.splice).asInstanceOf[Parse[Unit]]
      }
    }
  }

  def parsedSequence0[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
  (c: Context)
  (other: c.Expr[Parse[V]], cut: Boolean)
  (s: c.Expr[Implicits.Sequencer[T, V, R]],
   whitespace: Option[c.Expr[Parse[Any] => Parse[Unit]]]): c.Expr[Parse[R]] = {
    import c.universe._

    val lhs = c.prefix.asInstanceOf[Expr[EagerOps[T]]]
    val cut1 = c.Expr[Boolean](if(cut) q"true" else q"false")
    val consumeWhitespace = whitespace match{
      case None => reify{(c: Parse[Any]) => true}
      case Some(ws) =>
        if (ws.tree.tpe =:= typeOf[fasterparser.NoWhitespace.noWhitespaceImplicit.type]){
          reify{(c: Parse[Any]) => true}
        }else{
          reify{(c: Parse[Any]) => ws.splice(c); c.isSuccess}
        }
    }

    reify {
      {
        lhs.splice.parse0 match{ case ctx3 =>
          if (!ctx3.isSuccess) ctx3
          else {
            val pValue = ctx3.successValue
            val pCut = ctx3.cut
            val preWsIndex = ctx3.index
            if (!consumeWhitespace.splice(ctx3)) ctx3
            else {
              val preOtherIndex = ctx3.index
              other.splice
              val postOtherIndex = ctx3.index
              val nextIndex =
                if (preOtherIndex >= postOtherIndex && postOtherIndex < ctx3.input.length) preWsIndex
                else ctx3.index
              val rhsNewCut = cut1.splice | ctx3.cut | pCut
              if (!ctx3.isSuccess) ctx3.prepareFailure(ctx3.index, cut = rhsNewCut)
              else {
                ctx3.prepareSuccess(
                  s.splice.apply(pValue.asInstanceOf[T], ctx3.successValue.asInstanceOf[V]),
                  nextIndex,
                  cut = rhsNewCut
                )
              }
            }
          }

        }
      }.asInstanceOf[Parse[R]]
    }
  }

  def cutMacro[T: c.WeakTypeTag](c: Context): c.Expr[Parse[T]] = {
    import c.universe._
    val lhs = c.prefix.asInstanceOf[c.Expr[EagerOps[_]]]
    reify{
      val ctx1 = lhs.splice.parse0
      if (ctx1.isSuccess) ctx1.prepareSuccess(ctx1.successValue, cut = true).asInstanceOf[Parse[T]]
      else ctx1.prepareFailure(ctx1.index)
    }
  }


  def charsWhileInMacro1(c: Context)
                        (s: c.Expr[String])
                        (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[Unit]] = {
    import c.universe._
    charsWhileInMacro(c)(s, reify(1))(ctx)
  }
  def charsWhileInMacro(c: Context)
                       (s: c.Expr[String], min: c.Expr[Int])
                       (ctx: c.Expr[Parse[Any]]): c.Expr[Parse[Unit]] = {
    import c.universe._

    val literal = s.actualType match{
      case ConstantType(Constant(x: String)) => x
      case _ => c.abort(c.enclosingPosition, "Function can only accept constant singleton type")
    }

    val bracketed = c.Expr[String](Literal(Constant("[" + literal + "]")))

    val ctx1 = TermName(c.freshName("ctx"))
    val index = TermName(c.freshName("index"))
    val input = TermName(c.freshName("input"))
    val inputLength = TermName(c.freshName("inputLength"))
    val start = TermName(c.freshName("start"))
    val res = q"""
      val $ctx1 = $ctx
      var $index = $ctx1.index
      val $input = $ctx1.input
      val $inputLength = $input.length


      val $start = $index
      while(
        $index < $inputLength &&
        ${parseCharCls(c)(c.Expr[Char](q"$input($index)"), Seq(literal))}
      ) $index += 1
      if ($index - $start >= $min) $ctx1.freshSuccess((), "chars-while-in(" + $bracketed+ ", " + $min + ")", index = $index)
      else {
        if ($ctx.index == $ctx1.traceIndex) $ctx1.aggregateFailure($bracketed)
        $ctx1.shortFailureMsg = () => $bracketed
        $ctx1.isSuccess = false
        $ctx1.asInstanceOf[Parse[Unit]]
      }
    """
    c.Expr[Parse[Unit]](res)
  }


  def byNameOpsStrMacro(c: Context)
                       (parse0: c.Expr[String])
                       (ctx: c.Expr[Parse[Any]]): c.Expr[fasterparser.Parsing.ByNameOps[Unit]] = {
    import c.universe._
    val literal = MacroImpls.literalStrMacro(c)(parse0)(ctx)
    reify{ new fasterparser.Parsing.ByNameOps[Unit](() => literal.splice)}
  }


}
