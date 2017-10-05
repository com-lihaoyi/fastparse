package fasterparser

import scala.annotation.{switch, tailrec}
import scala.collection.mutable


object Parse {
  def main(args: Array[String]): Unit = {
    def hello[_:Ctx] = P( "hello" )
    def world[_:Ctx] = P( "world" )
    def helloWorld[_:Ctx] = P( hello.! ~ (" " | ",").rep ~ world.! )

    println(hello("hello "))          // Parsed.Success(())
    println(helloWorld("hello "))     // Parsed.Failure(6)
    println(helloWorld("hello world"))// Parsed.Success((hello,world))
    println(helloWorld("hello   world"))// Parsed.Success((hello,world))
    println(helloWorld("hello, world"))// Parsed.Success((hello,world))
  }

  type P[+T] = Parsed[T]
  def P[T](t: => Parsed[T])(implicit ctx: Ctx[_], name: sourcecode.Name): Parsed[T] = {
    t match{
      case f: Parsed.Failure =>
        f.stack = name.value :: f.stack
        f
      case s: Parsed.Success[_] => s
    }
  }

  implicit def strToParsed(s: String)(implicit ctx: Ctx[_]): Parsed[Unit] = {

    if (ctx.input.startsWith(s, ctx.success.index)) ctx.freshSuccess((), ctx.success.index + s.length)
    else ctx.freshFailure()
  }

  implicit class EagerOps[S, T](parse0: S)(implicit conv: S => Parsed[T], ctx: Ctx[_]){

    def ~/[V, R](other: => Parsed[V])(implicit s: Implicits.Sequencer[T, V, R]): Parsed[R] = {
      this.~(other, cut = true)
    }
    def ~/ : Parsed[T] = {
      conv(parse0) match{
        case f: Parsed.Failure => ctx.prepareFailure(f.index)
        case p: Parsed.Success[T] => ctx.prepareSuccess(p.value, cut = true)
      }
    }
    def ~[V, R](other: => Parsed[V], cut: Boolean = false)(implicit s: Implicits.Sequencer[T, V, R]): Parsed[R] = {
      conv(parse0) match{
        case f: Parsed.Failure => f
        case p: Parsed.Success[T] =>
          val pValue = p.value
          val pCut = p.cut
          other match{
            case f: Parsed.Failure =>
              ctx.prepareFailure(f.index, cut = f.cut | cut | pCut)
            case p1: Parsed.Success[V] =>
              ctx.prepareSuccess(s.apply(pValue, p1.value), cut = pCut | cut | p1.cut)

          }
      }
    }
  }

  implicit class ByNameOps[S, T](parse0: => S)(implicit conv: S => Parsed[T], ctx: Ctx[_]){
    def rep[V](implicit repeater: Implicits.Repeater[T, V]): Parsed[V] = rep(sep=null)
    def rep[V](min: Int = 0,
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
        else conv(parse0) match{
          case f: Parsed.Failure =>

            if (f.cut | precut) f
            else end(startIndex, startIndex, count)


          case s: Parsed.Success[T] =>
            val beforeSepIndex = ctx.success.index
            repeater.accumulate(s.value, acc)
            if (count + 1 == actualMax) end(beforeSepIndex, beforeSepIndex, count + 1)
            else sep match{
              case null => rec(beforeSepIndex, count+1, false)
              case s: Parsed.Success[_] => rec(beforeSepIndex, count+1, s.cut)
              case f: Parsed.Failure =>
                if (f.cut) ctx.prepareFailure(beforeSepIndex)
                else end(beforeSepIndex, beforeSepIndex, count+1)
            }
        }
      }

      val res = rec(ctx.success.index, 0, false)

      res
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

        output(s"$indent+$msg:${ctx.success.index}")
        val depth = ctx.logDepth
        ctx.logDepth += 1
        val res = conv(parse0)
        ctx.logDepth = depth
        val strRes = res match{
          case s: Parsed.Success[T] =>
            s"Success(${literalize(ctx.input.slice(s.index, s.index + 20))}${if (s.cut) ", cut" else ""})"
          case f: Parsed.Failure =>
//            val stack = Failure.filterFullStack(f.fullStack)
//            val trace = Failure.formatStackTrace(
//              stack.reverse,
//              f.input,
//              index,
//              Failure.formatParser(f.lastParser, f.input, f.index)
//            )
            val trace = ""
            s"Failure($trace${if (f.cut) ", cut" else ""})"
        }
        output(s"$indent-$msg:${res.index}:$strRes")
//        output(s"$indent-$msg:${repr.prettyIndex(cfg.input, index)}:$strRes")
        res
      }
    }

    def ! : Parsed[String] = {
      val startPos = ctx.success.index

      conv(parse0) match{
        case f: Parsed.Failure => f
        case s: Parsed.Success[_] => ctx.freshSuccess(ctx.input.substring(startPos, ctx.success.index))
      }
    }

    def unary_! : Parsed[Unit] = {
      val startPos = ctx.success.index
      conv(parse0) match{
        case f: Parsed.Failure => ctx.freshSuccess((), startPos)
        case s: Parsed.Success[_] => ctx.prepareFailure(startPos)
      }
    }

    def ?[V](implicit optioner: Implicits.Optioner[T, V]): Parsed[V] = {
      val startPos = ctx.success.index

      conv(parse0) match{
        case f: Parsed.Failure =>
          if (f.cut) f
          else ctx.freshSuccess(optioner.none, startPos)

        case s: Parsed.Success[T] => ctx.prepareSuccess(optioner.some(s.value))
      }
    }

    def |[V >: T](other: => Parsed[V]): Parsed[V] = {
      val startPos = ctx.success.index
      val res = conv(parse0) match {
        case p: Parsed.Success[T] => p
        case f: Parsed.Failure =>
          ctx.success.index = startPos
          if (f.cut) f
          else other match{
            case p: Parsed.Success[V] => p
            case f: Parsed.Failure => ctx.freshFailure(startPos)
          }
      }
      res
    }
  }

  def &(parse: => Parsed[_])(implicit ctx: Ctx[_]): Parsed[Unit] = {
    val startPos = ctx.success.index
    parse match{
      case f: Parsed.Failure => f
      case s: Parsed.Success[_] => ctx.prepareSuccess((), startPos)
    }
  }

  def CharsWhileIn(s: String)(implicit ctx: Ctx[_]) = CharsWhile(s.toSet)

  def CharIn(s: CharSequence*)(implicit ctx: Ctx[_]) = {
    CharPred{c =>
      s.exists { cs =>
        var success = false
        var index = 0
        while(!success && index < cs.length){
          if (cs.charAt(index) == c) success = true
          index += 1
        }
        success
      }
    }
  }

  def End(implicit ctx: Ctx[_]): Parsed[Unit] = {
    if (ctx.success.index == ctx.input.length) ctx.freshSuccess(())
    else ctx.freshFailure()
  }

  def Start(implicit ctx: Ctx[_]): Parsed[Unit] = {
    if (ctx.success.index == 0) ctx.freshSuccess(())
    else ctx.freshFailure()

  }

  def Pass(implicit ctx: Ctx[_]): Parsed[Unit] = ctx.freshSuccess(())

  def Fail(implicit ctx: Ctx[_]): Parsed[Unit] = ctx.freshFailure()

  def Index(implicit ctx: Ctx[_]): Parsed[Int] = ctx.freshSuccess(ctx.success.index)

  def AnyChar(implicit ctx: Ctx[_]): Parsed[Unit] = CharPred(_ => true)
  def CharPred(p: Char => Boolean)(implicit ctx: Ctx[_]): Parsed[Unit] = {
    if (!ctx.input.lift(ctx.success.index).exists(p)) ctx.freshFailure()
    else ctx.freshSuccess((), ctx.success.index + 1)
  }
  def CharsWhile(p: Char => Boolean)(implicit ctx: Ctx[_]) = {
    def currentCharMatches = ctx.input.lift(ctx.success.index).exists(p)
    if (!currentCharMatches)  {
      ctx.isSuccess = false
      ctx.failure
    }else{
      while(currentCharMatches) ctx.success.index += 1
      ctx.freshSuccess(())
    }
  }


  implicit def parseInputCtx(s: String): Ctx[_] = Ctx(s)
}

class Ctx[+_](val input: String,
              val stack: mutable.Buffer[String],
              val success: Parsed.Success[_],
              val failure: Parsed.Failure,
              var isSuccess: Boolean,
              var logDepth: Int){

  def freshSuccess[T](value: T, index: Int = success.index) = prepareSuccess(value, index, cut = false)
  def prepareSuccess[T](value: T, index: Int = success.index, cut: Boolean = success.cut): Parsed.Success[T] = {
    val res = success.asInstanceOf[Parsed.Success[T]]
    isSuccess = true
    res.value = value
    res.index = index
    res.cut = cut
    res
  }
  def freshFailure(startPos: Int = success.index): Parsed.Failure = {
    prepareFailure(startPos, cut = false)
  }

  def prepareFailure(index: Int, cut: Boolean = failure.cut): Parsed.Failure = {
    isSuccess = false
    failure.stack = Nil
    failure.index = index
    failure.cut = cut
    failure
  }
}
object Ctx{
  def apply(input: String) = new Ctx(
    input = input,
    stack = mutable.Buffer.empty,
    success = new Parsed.Success,
    failure = new Parsed.Failure,
    isSuccess = true,
    logDepth = 0
  )
}

abstract class Parsed[+T](val isSuccess: Boolean){
  var cut: Boolean = false
  var index: Int = 0
  def map[V](f: T => V): Parsed[V]
  def flatMap[V](f: T => Parsed[V]): Parsed[V]
  def filter(f: T => Boolean): Parsed[T]
}

object Parsed{
  object Success{
    def unapply[T](x: Parsed[T]): Option[(T, Int)] = x match{
      case s: Success[T] => Some((s.value, s.index))
      case f: Failure => None
    }
  }
  object Failure{
    def unapply[T](x: Parsed[T]): Option[(Unit, Int, Unit)] = x match{
      case s: Failure => Some(((), s.index, ()))
      case f: Success[T] => None
    }
  }
  class Success[T] extends Parsed[T](true){
    var value: T = null.asInstanceOf[T]
    def map[V](f: T => V) = {
      val this2 = this.asInstanceOf[Success[V]]
      this2.value = f(value)
      this2
    }
    def flatMap[V](f: T => Parsed[V]): Parsed[V] = {
      f(value) match{
        case f: Failure => f
        case s: Success[V] => s
      }
    }
    def filter(f: T => Boolean): Parsed[T] = {
      if (f(value)) this
      else {
        val f = new Failure()
        f.index = index
        f
      }
    }
    override def toString() = s"Parsed.Success($value)"
  }
  class Failure extends Parsed[Nothing](false){
    var stack = List.empty[String]
    override def toString() = s"Parsed.Failure($index)"
    def map[V](f: Nothing => V) = this
    def flatMap[V](f: Nothing => Parsed[V]): Parsed[V] = this
    def filter(f: Nothing => Boolean) = this
  }
}