package fasterparser

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import language.experimental.macros
import reflect.macros.blackbox.Context

object Parse {

  type P[+T] = Parsed[T]

    def P[T](t: Parsed[T])(implicit name: sourcecode.Name): Parsed[T] = macro pMacro[T]

  def pMacro[T: c.WeakTypeTag](c: Context)
                              (t: c.Expr[Parsed[T]])
                              (name: c.Expr[sourcecode.Name]): c.Expr[Parsed[T]] = {

    import c.universe._
    reify[Parsed[T]]{
      t.splice match{
        case ctx0 =>
          if (!ctx0.isSuccess){
            ctx0.failureStack = name.splice.value :: ctx0.failureStack
          }
          ctx0.asInstanceOf[Parsed[T]]
      }
    }
  }

  implicit def LiteralStr(s: String)(implicit ctx: Parsed[_]): Parsed[Unit] = {

    if (ctx.input.startsWith(s, ctx.successIndex)) ctx.freshSuccess((), ctx.successIndex + s.length)
    else ctx.freshFailure().asInstanceOf[Parsed[Unit]]
  }
  def literalStrMacro(c: Context)(s: c.Expr[String])(ctx: c.Expr[Parsed[Any]]): c.Expr[Parsed[Unit]] = {
    import c.universe._
    reify{
      val s1 = s.splice
      if (ctx.splice.input.startsWith(s1, ctx.splice.successIndex)) ctx.splice.freshSuccess((), ctx.splice.successIndex + s1.length)
      else ctx.splice.freshFailure().asInstanceOf[Parsed[Unit]]
    }
  }

  def cutMacro[T: c.WeakTypeTag](c: Context): c.Expr[Parsed[T]] = {
    import c.universe._
    reify{

      val ctx1 = c.prefix.splice.asInstanceOf[Parsed[Any]]
      if (ctx1.isSuccess) ctx1.prepareSuccess(ctx1.successValue, cut = true).asInstanceOf[Parsed[T]]
      else ctx1.prepareFailure(ctx1.failureIndex)
    }
  }
  def cutStrMacro(c: Context)(ctx: c.Expr[Parsed[Any]]): c.Expr[Parsed[Unit]] = {
    import c.universe._
    reify{
      val ctx2 = LiteralStr(c.prefix.splice.asInstanceOf[EagerOpsStr].parse0)(ctx.splice)

      if (ctx2.isSuccess) ctx2.prepareSuccess(ctx2.successValue, cut = true)
      else ctx2.prepareFailure(ctx2.failureIndex)
    }
  }

  implicit class EagerOpsStr(val parse0: String) extends AnyVal {

    def ~/[V, R](other: Parsed[V])
                   (implicit s: Implicits.Sequencer[Unit, V, R],
                    whitespace: Parsed[Any] => Parsed[Unit],
                    ctx: Parsed[Any]): Parsed[R] = macro parsedSequenceCutString[V, R]


    def /[T](implicit  ctx: Parsed[_]): Parsed[Unit] = macro cutStrMacro

    def ~[V, R](other: Parsed[V])
                  (implicit s: Implicits.Sequencer[Unit, V, R],
                   whitespace: Parsed[Any] => Parsed[Unit],
                   ctx: Parsed[Any]): Parsed[R] = macro parsedSequenceString[V, R]


    def ~~/[V, R](other: Parsed[V])
                    (implicit s: Implicits.Sequencer[Unit, V, R],
                     ctx: Parsed[Any]): Parsed[R] = macro parsedSequenceCutString1[V, R]


    def ~~[V, R](other: Parsed[V])
                (implicit s: Implicits.Sequencer[Unit, V, R],
                 ctx: Parsed[Any]): Parsed[R] = macro parsedSequenceString1[V, R]
  }

  def parsedSequence0[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                     (c: Context)
                     (lhs: c.Expr[Parsed[T]], other: c.Expr[Parsed[V]], cut: Boolean)
                     (s: c.Expr[Implicits.Sequencer[T, V, R]],
                      whitespace: c.Expr[Parsed[Any] => Parsed[Unit]]): c.Expr[Parsed[R]] = {
    import c.universe._

    val cut1 = c.Expr[Boolean](if(cut) q"true" else q"false")
    reify {
      def wrap() = {
        lhs.splice match{
          case ctx3 =>
            if (!ctx3.isSuccess) ctx3
            else {
              val pValue = ctx3.successValue
              val pCut = ctx3.successCut
              whitespace.splice(ctx3)
              if (!ctx3.isSuccess) ctx3
              else {
                other.splice
                if (!ctx3.isSuccess){
                  ctx3.prepareFailure(ctx3.failureIndex, cut = ctx3.failureCut | cut1.splice | pCut)
                }else {
                  ctx3.prepareSuccess(
                    s.splice.apply(pValue.asInstanceOf[T], ctx3.successValue.asInstanceOf[V]),
                    cut = pCut | cut1.splice | ctx3.successCut
                  )
                }
              }
            }

        }
      }
      wrap().asInstanceOf[Parsed[R]]
    }
  }

  def parsedSequence00[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                     (c: Context)
                     (lhs: c.Expr[Parsed[T]], other: c.Expr[Parsed[V]], cut: Boolean)
                     (s: c.Expr[Implicits.Sequencer[T, V, R]]): c.Expr[Parsed[R]] = {
    import c.universe._

    val cut1 = c.Expr[Boolean](if (cut) q"true" else q"false")
    reify {
      def wrap() = {
        lhs.splice match{
          case ctx4 =>
            if (!ctx4.isSuccess) ctx4
            else {
              val pValue = ctx4.successValue
              val pCut = ctx4.successCut
              other.splice
              if (!ctx4.isSuccess) {
                ctx4.prepareFailure(ctx4.failureIndex, cut = ctx4.failureCut | cut1.splice | pCut)
              } else {
                ctx4.prepareSuccess(
                  s.splice.apply(pValue.asInstanceOf[T], ctx4.successValue.asInstanceOf[V]),
                  cut = pCut | cut1.splice | ctx4.successCut
                )
              }
            }
        }
      }

      wrap().asInstanceOf[Parsed[R]]
    }
  }

  def parsedSequenceString[V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parsed[V]])
                    (s: c.Expr[Implicits.Sequencer[Unit, V, R]],
                     whitespace: c.Expr[Parsed[Any] => Parsed[Unit]],
                     ctx: c.Expr[Parsed[Any]]): c.Expr[Parsed[R]] = {
    import c.universe._
    val lhs = literalStrMacro(c)(reify(c.prefix.asInstanceOf[Expr[EagerOpsStr]].splice.parse0))(ctx)
    parsedSequence0[Unit, V, R](c)(lhs, other, false)(s, whitespace)
  }
  def parsedSequenceCutString[V: c.WeakTypeTag, R: c.WeakTypeTag]
                      (c: Context)
                      (other: c.Expr[Parsed[V]])
                      (s: c.Expr[Implicits.Sequencer[Unit, V, R]],
                       whitespace: c.Expr[Parsed[Any] => Parsed[Unit]],
                       ctx: c.Expr[Parsed[Any]]): c.Expr[Parsed[R]] = {
    import c.universe._
    val lhs = literalStrMacro(c)(reify(c.prefix.asInstanceOf[Expr[EagerOpsStr]].splice.parse0))(ctx)
    parsedSequence0[Unit, V, R](c)(lhs, other, true)(s, whitespace)
  }
  def parsedSequenceString1[V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parsed[V]])
                    (s: c.Expr[Implicits.Sequencer[Unit, V, R]],
                     ctx: c.Expr[Parsed[Any]]): c.Expr[Parsed[R]] = {
    import c.universe._
    val lhs = literalStrMacro(c)(reify(c.prefix.asInstanceOf[Expr[EagerOpsStr]].splice.parse0))(ctx)
    parsedSequence00[Unit, V, R](c)(lhs, other, false)(s)
  }
  def parsedSequenceCutString1[V: c.WeakTypeTag, R: c.WeakTypeTag]
                      (c: Context)
                      (other: c.Expr[Parsed[V]])
                      (s: c.Expr[Implicits.Sequencer[Unit, V, R]],
                       ctx: c.Expr[Parsed[Any]]): c.Expr[Parsed[R]] = {
    import c.universe._
    val lhs = literalStrMacro(c)(reify(c.prefix.asInstanceOf[Expr[EagerOpsStr]].splice.parse0))(ctx)
    parsedSequence00[Unit, V, R](c)(lhs, other, true)(s)
  }
  def parsedSequence[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parsed[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]],
                     whitespace: c.Expr[Parsed[Any] => Parsed[Unit]]): c.Expr[Parsed[R]] = {
    import c.universe._
    val lhs = reify{c.prefix.asInstanceOf[Expr[EagerOps[T]]].splice.parse0}
    parsedSequence0[T, V, R](c)(lhs, other, false)(s, whitespace)
  }
  def parsedSequenceCut[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parsed[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]],
                     whitespace: c.Expr[Parsed[Any] => Parsed[Unit]]): c.Expr[Parsed[R]] = {
    import c.universe._
    val lhs = reify{c.prefix.asInstanceOf[Expr[EagerOps[T]]].splice.parse0}
    parsedSequence0[T, V, R](c)(lhs, other, true)(s, whitespace)
  }
  def parsedSequence1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parsed[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]]): c.Expr[Parsed[R]] = {
    import c.universe._
    val lhs = reify{c.prefix.asInstanceOf[Expr[EagerOps[T]]].splice.parse0}
    parsedSequence00[T, V, R](c)(lhs, other, false)(s)
  }
  def parsedSequenceCut1[T: c.WeakTypeTag, V: c.WeakTypeTag, R: c.WeakTypeTag]
                    (c: Context)
                    (other: c.Expr[Parsed[V]])
                    (s: c.Expr[Implicits.Sequencer[T, V, R]]): c.Expr[Parsed[R]] = {
    import c.universe._
    val lhs = reify{c.prefix.asInstanceOf[Expr[EagerOps[T]]].splice.parse0}
    parsedSequence00[T, V, R](c)(lhs, other, true)(s)
  }


  implicit def EagerOps[T](parse0: Parsed[T]) = new EagerOps(parse0)
  class EagerOps[T](val parse0: Parsed[T]) {

    def ~/[V, R](other: Parsed[V])
                (implicit s: Implicits.Sequencer[T, V, R],
                 whitespace: Parsed[Any] => Parsed[Unit]): Parsed[R] = macro parsedSequenceCut[T, V, R]

    def / : Parsed[T] = macro cutMacro[T]

    def ~[V, R](other:  Parsed[V])
               (implicit s: Implicits.Sequencer[T, V, R],
                whitespace: Parsed[Any] => Parsed[Unit]): Parsed[R] = macro parsedSequence[T, V, R]


    def ~~/[V, R](other: Parsed[V])
                  (implicit s: Implicits.Sequencer[T, V, R]): Parsed[R] = macro parsedSequenceCut1[T, V, R]


    def ~~[V, R](other: Parsed[V])
                (implicit s: Implicits.Sequencer[T, V, R]): Parsed[R] = macro parsedSequence1[T, V, R]

    def map[V](f: T => V): Parsed[V] = {
      if (!parse0.isSuccess) parse0.asInstanceOf[Parsed[V]]
      else {
        val this2 = parse0.asInstanceOf[Parsed[V]]
        this2.successValue = f(this2.successValue.asInstanceOf[T])
        this2
      }
    }
    def flatMap[V](f: T => Parsed[V]): Parsed[V] = {
      if (!parse0.isSuccess) parse0.asInstanceOf[Parsed[V]]
      else  f(parse0.successValue.asInstanceOf[T])
    }
  }


  def eitherMacro[S: c.WeakTypeTag, T: c.WeakTypeTag, V >: T: c.WeakTypeTag]
                 (c: Context)
                 (other: c.Expr[Parsed[V]]): c.Expr[Parsed[V]] = {
    import c.universe._

    val q"fasterparser.Parse.ByNameOps[$k, $v]($parse0)($conv, $ctx)" = c.prefix.tree
    reify {
      val ctx5: Parsed[Any] = c.Expr[Parsed[Any]](ctx).splice
      val startPos = ctx5.successIndex
      val conv1 = c.Expr[S => Parsed[T]](conv).splice
      val parse00 = c.Expr[S](parse0).splice
      def wrap() = {

        conv1(parse00)
        if (ctx5.isSuccess) ctx5
        else{
          ctx5.successIndex = startPos
          if (ctx5.failureCut) ctx5
          else {
            other.splice
            if (ctx5.isSuccess) ctx5
            else ctx5.freshFailure(startPos)

          }
        }
      }
      wrap().asInstanceOf[Parsed[V]]
    }
  }
  def captureMacro[S: c.WeakTypeTag, T: c.WeakTypeTag](c: Context): c.Expr[Parsed[String]] = {
    import c.universe._

    val q"fasterparser.Parse.ByNameOps[$k, $v]($parse0)($conv, $ctx)" = c.prefix.tree

    reify {
      val ctx6 = c.Expr[Parsed[Any]](ctx).splice
      val startPos = ctx6.successIndex
      val conv1 = c.Expr[S => Parsed[T]](conv).splice
      val parse00: S = c.Expr[S](parse0).splice

      conv1(parse00)
      if (!ctx6.isSuccess) ctx6.asInstanceOf[Parsed[String]]
      else ctx6.freshSuccess(ctx6.input.substring(startPos, ctx6.successIndex)).asInstanceOf[Parsed[String]]
    }
  }
  implicit class ByNameOps[S, T](parse0: => S)(implicit val conv: S => Parsed[T], val ctx: Parsed[Any]){
    def parse00 = parse0
    def |[V >: T](other: Parsed[V]): Parsed[V] = macro eitherMacro[S, T, V]
    def repX[V](implicit repeater: Implicits.Repeater[T, V]): Parsed[V] = repX(sep=null)
    def repX[V](min: Int = 0,
               max: Int = Int.MaxValue,
               exactly: Int = -1,
               sep: => Parsed[_] = null)(implicit repeater: Implicits.Repeater[T, V]): Parsed[V] = {


      val acc = repeater.initial
      val actualMin = if(exactly == -1) min else exactly
      val actualMax = if(exactly == -1) max else exactly
      def end(successIndex: Int, failureIndex: Int, count: Int) = {
        if (count < actualMin) ctx.prepareFailure(failureIndex)
        else ctx.prepareSuccess(repeater.result(acc), successIndex)
      }
      @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parsed[V] = {
        if (count == 0 && actualMax == 0) ctx.prepareSuccess(repeater.result(acc), startIndex)
        else {
          conv(parse0)
          if (!ctx.isSuccess) {
            if (ctx.failureCut | precut) ctx.asInstanceOf[Parsed[V]]
            else end(startIndex, startIndex, count)
          }else {
            val beforeSepIndex = ctx.successIndex
            repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
            if (count + 1 == actualMax) end(beforeSepIndex, beforeSepIndex, count + 1)
            else if (sep == null) rec(beforeSepIndex, count+1, false)
            else {
              if (ctx.isSuccess) rec(beforeSepIndex, count+1, ctx.successCut)
              else if (ctx.failureCut) ctx.prepareFailure(beforeSepIndex)
              else end(beforeSepIndex, beforeSepIndex, count+1)
            }
          }
        }
      }

      val res = rec(ctx.successIndex, 0, false)

      res
    }
    def rep[V](implicit repeater: Implicits.Repeater[T, V],
               whitespace: Parsed[_] => Parsed[Unit]): Parsed[V] = rep(sep=null)
    def rep[V](min: Int = 0,
               max: Int = Int.MaxValue,
               exactly: Int = -1,
               sep: => Parsed[_] = null)
              (implicit repeater: Implicits.Repeater[T, V],
               whitespace: Parsed[_] => Parsed[Unit]): Parsed[V] = {


      val acc = repeater.initial
      val actualMin = if(exactly == -1) min else exactly
      val actualMax = if(exactly == -1) max else exactly
      def end(successIndex: Int, failureIndex: Int, count: Int) = {
        if (count < actualMin) ctx.prepareFailure(failureIndex)
        else ctx.prepareSuccess(repeater.result(acc), successIndex)
      }
      @tailrec def rec(startIndex: Int, count: Int, precut: Boolean): Parsed[V] = {
        whitespace(ctx)
        if (!ctx.isSuccess) ctx.asInstanceOf[Parsed[V]]
        else{
          if (count == 0 && actualMax == 0) ctx.prepareSuccess(repeater.result(acc), startIndex)
          else {
            conv(parse0)
            if (!ctx.isSuccess){
              if (ctx.failureCut | precut) ctx.asInstanceOf[Parsed[V]]
              else end(startIndex, startIndex, count)
            }else{
              val beforeSepIndex = ctx.successIndex
              repeater.accumulate(ctx.successValue.asInstanceOf[T], acc)
              if (count + 1 == actualMax) end(beforeSepIndex, beforeSepIndex, count + 1)
              else if (sep == null) rec(beforeSepIndex, count+1, false)
              else {
                if (ctx.isSuccess) rec(beforeSepIndex, count+1, ctx.successCut)
                else if (ctx.failureCut) ctx.prepareFailure(beforeSepIndex)
                else end(beforeSepIndex, beforeSepIndex, count+1)
              }

            }
          }
        }
      }
      rec(ctx.successIndex, 0, false)
    }

    def literalize(s: IndexedSeq[Char], unicode: Boolean = true) = {
      val sb = new StringBuilder
      sb.append('"')
      var i = 0
      val len = s.length
      while (i < len) {
        (s(i): @switch) match {
          case '"' => sb.append("\\\"")
          case '\\' => sb.append("\\\\")
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case c =>
            if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
            else sb.append(c)
        }
        i += 1
      }
      sb.append('"')

      sb.result()
    }

    def log(implicit name: sourcecode.Name): Parsed[T] = {
      if (ctx.logDepth == -1) conv(parse0)
      else{
        val msg = name.value
        val output = println(_: String)
        val indent = "  " * ctx.logDepth

        output(s"$indent+$msg:${ctx.successIndex}")
        val depth = ctx.logDepth
        ctx.logDepth += 1
        conv(parse0)
        ctx.logDepth = depth
        val (index, strRes) = if (ctx.isSuccess){
          ctx.successIndex -> s"Success(${literalize(ctx.input.slice(ctx.successIndex, ctx.successIndex + 20))}${if (ctx.successCut) ", cut" else ""})"
        } else{
          val trace = ""
          ctx.failureIndex -> s"Failure($trace${if (ctx.failureCut) ", cut" else ""})"
        }
        output(s"$indent-$msg:$index:$strRes")
//        output(s"$indent-$msg:${repr.prettyIndex(cfg.input, index)}:$strRes")
        ctx.asInstanceOf[Parsed[T]]
      }
    }

    def ! : Parsed[String] = macro captureMacro[S, T]

    def unary_! : Parsed[Unit] = {
      val startPos = ctx.successIndex
      conv(parse0)
      if (!ctx.isSuccess) ctx.freshSuccess((), startPos)
      else ctx.prepareFailure(startPos)
    }

    def ?[V](implicit optioner: Implicits.Optioner[T, V]): Parsed[V] = {
      val startPos = ctx.successIndex
      conv(parse0)
      if (ctx.isSuccess) ctx.prepareSuccess(optioner.some(ctx.successValue.asInstanceOf[T]))
      else if (ctx.failureCut) ctx.asInstanceOf[Parsed[V]]
      else ctx.freshSuccess(optioner.none, startPos)
    }


    def filter(f: T => Boolean): Parsed[T] = {
      conv(parse0)
      if (!ctx.isSuccess) ctx.asInstanceOf[Parsed[T]]
      else if (f(ctx.successValue.asInstanceOf[T])) ctx.asInstanceOf[Parsed[T]]
      else ctx.freshFailure().asInstanceOf[Parsed[T]]
    }
  }

  def &(parse: => Parsed[_])(implicit ctx: Parsed[_]): Parsed[Unit] = {

    val startPos = ctx.successIndex
    parse
    if (ctx.isSuccess) ctx.prepareSuccess((), startPos)
    else ctx.asInstanceOf[Parsed[Unit]]

  }

  def End(implicit ctx: Parsed[_]): Parsed[Unit] = {
    if (ctx.successIndex == ctx.input.length) ctx.freshSuccess(())
    else ctx.freshFailure().asInstanceOf[Parsed[Unit]]
  }

  def Start(implicit ctx: Parsed[_]): Parsed[Unit] = {
    if (ctx.successIndex == 0) ctx.freshSuccess(())
    else ctx.freshFailure().asInstanceOf[Parsed[Unit]]

  }

  def Pass(implicit ctx: Parsed[_]): Parsed[Unit] = ctx.freshSuccess(())

  def Fail(implicit ctx: Parsed[_]): Parsed[Nothing] = ctx.freshFailure().asInstanceOf[Parsed[Nothing]]

  def Index(implicit ctx: Parsed[_]): Parsed[Int] = ctx.freshSuccess(ctx.successIndex)

  def AnyChar(implicit ctx: Parsed[_]): Parsed[Unit] = CharPred(_ => true)
  def CharPred(p: Char => Boolean)(implicit ctx: Parsed[_]): Parsed[Unit] = {
    if (!(ctx.successIndex < ctx.input.length && p(ctx.input(ctx.successIndex)))) ctx.freshFailure().asInstanceOf[Parsed[Unit]]
    else ctx.freshSuccess((), ctx.successIndex + 1)
  }
  def CharsWhile(p: Char => Boolean, min: Int = 1)(implicit ctx: Parsed[_]): Parsed[Unit] = {
    def currentCharMatches = {
      val index = ctx.successIndex
      val input = ctx.input
      index < input.length && p(input(index))
    }

    if (!currentCharMatches)  {
      ctx.isSuccess = false
      if (min == 0) ctx.freshSuccess(()) else ctx.asInstanceOf[Parsed[Unit]]
    }else{
      val start = ctx.successIndex
      while(currentCharMatches) ctx.successIndex += 1
      if (ctx.successIndex - start >= min) ctx.freshSuccess(()) else ctx.asInstanceOf[Parsed[Unit]]
    }
  }


  def parseInputCtx(s: String): Parsed[_] = Parsed(s)
}

class Parsed[+T](val input: String,
                 var failureStack: List[String],
                 var isSuccess: Boolean,
                 var logDepth: Int,
                 var successIndex: Int,
                 var failureIndex: Int,
                 var successCut: Boolean,
                 var failureCut: Boolean,
                 var successValue: Any){

  def freshSuccess[V](value: V, index: Int = successIndex) = prepareSuccess(value, index, cut = false)
  def prepareSuccess[V](value: V, index: Int = successIndex, cut: Boolean = successCut): Parsed[V] = {

    isSuccess = true
    successValue = value
    successIndex = index
    successCut = cut
    this.asInstanceOf[Parsed[V]]
  }
  def freshFailure(startPos: Int = successIndex): Parsed[Nothing] = {
    prepareFailure(startPos, cut = false)
  }

  def prepareFailure(index: Int, cut: Boolean = failureCut): Parsed[Nothing] = {
    isSuccess = false
    failureStack = Nil
    failureIndex = index
    failureCut = cut
    this.asInstanceOf[Parsed[Nothing]]
  }

  def result: Result[T] = {
    if (isSuccess) Result.Success(successValue.asInstanceOf[T], successIndex)
    else Result.Failure(failureIndex, failureStack)
  }
}
object Parsed{
  def apply(input: String) = new Parsed(
    input = input,
    failureStack = List.empty,
    isSuccess = true,
    logDepth = 0,
    0, 0, false, false, ()
  )
}

abstract class Result[+T](val isSuccess: Boolean){

  def get: Result.Success[T]
}

object Result{
  object Success{
    def unapply[T](x: Result[T]): Option[(T, Int)] = x match{
      case s: Success[T] => Some((s.value, s.index))
      case f: Failure => None
    }
  }
  object Failure{
    def unapply[T](x: Result[T]): Option[(Unit, Int, Unit)] = x match{
      case s: Failure => Some(((), s.index, ()))
      case f: Success[T] => None
    }
  }
  case class Success[+T](value: T, index: Int) extends Result[T](true){
    def get = this

    override def toString() = s"Result.Success($value)"
  }
  case class Failure(index: Int, stack: List[String]) extends Result[Nothing](false){
    def get = throw new Exception("Parse Error at " + index + ":\n" + stack.mkString("\n"))

    override def toString() = s"Result.Failure($index)"
  }
}