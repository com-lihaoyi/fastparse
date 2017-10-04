package fasterparser

import scala.annotation.tailrec
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

    println(Json.jsonExpr("31337"))
    println(Json.jsonExpr("31337"))
    println(Json.jsonExpr("\"31337\""))
    println(Json.jsonExpr("[true, 123]"))
    println(Json.jsonExpr("""{"hello": [true, 123], "world": {"foo": {"bar": "baz"}}}"""))
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

    if (ctx.input.startsWith(s, ctx.success.index)) {
      ctx.prepareSuccess((), ctx.success.index + s.length)
    }else{
      ctx.prepareFailure(ctx.success.index)
    }
  }

  implicit class EagerOps[S, T](parse0: S)(implicit conv: S => Parsed[T], ctx: Ctx[_]){

    def ~/[V, R](other: => Parsed[V])(implicit s: Implicits.Sequencer[T, V, R]): Parsed[R] = {
      this.~(other, cut = true)
    }
    def ~/ : Parsed[T] = {
      conv(parse0) match{
        case f: Parsed.Failure => f
        case p: Parsed.Success[T] =>
          ctx.cut = true
          p
      }
    }
    def ~[V, R](other: => Parsed[V], cut: Boolean = false)(implicit s: Implicits.Sequencer[T, V, R]): Parsed[R] = {
      conv(parse0) match{
        case f: Parsed.Failure => f
        case p: Parsed.Success[T] =>
          ctx.cut = true
          val pValue = p.value
          other match{
            case f: Parsed.Failure => f
            case p: Parsed.Success[V] => ctx.prepareSuccess(s.apply(pValue, p.value))

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
      @tailrec def rec(count: Int): Parsed[V] = {
        ctx.cut = false
        val beforeRepIndex = ctx.success.index
        if (count == 0 && actualMax == 0) ctx.prepareSuccess(repeater.result(acc), beforeRepIndex)
        else conv(parse0) match{
          case f: Parsed.Failure =>

            ctx.cut = false
            if (ctx.cut) ctx.prepareFailure(beforeRepIndex)
            else end(beforeRepIndex, beforeRepIndex, count)


          case s: Parsed.Success[T] =>
            val beforeSepIndex = ctx.success.index
            ctx.cut = false
            repeater.accumulate(s.value, acc)
            if (count + 1 == actualMax) end(beforeSepIndex, beforeSepIndex, count + 1)
            else sep match{
              case null => rec(count+1)
              case sepRes => sepRes match{
                case s: Parsed.Success[_] => rec(count+1)
                case f: Parsed.Failure =>
                  if (ctx.cut) ctx.prepareFailure(beforeSepIndex)
                  else end(beforeSepIndex, beforeSepIndex, count+1)
              }
            }
        }
      }
      val res = rec(0)
      ctx.cut = false
      res
    }

    def log()(implicit name: sourcecode.Name): Parsed[T] = {
      println("Starting " + name.value + " " + ctx.success.index)
      val res = conv(parse0)
      println("Ending   " + name.value + " " + ctx.success.index + " " + ctx.isSuccess)
      res
    }

    def ! : Parsed[String] = {
      val startPos = ctx.success.index

      conv(parse0) match{
        case f: Parsed.Failure => f
        case s: Parsed.Success[_] =>
          ctx.prepareSuccess(ctx.input.substring(startPos, ctx.success.index))
      }
    }

    def unary_! : Parsed[Unit] = {
      val startPos = ctx.success.index
      conv(parse0) match{
        case f: Parsed.Failure => ctx.prepareSuccess((), startPos)
        case s: Parsed.Success[_] => ctx.prepareFailure(startPos)
      }
    }

    def ?[V](implicit optioner: Implicits.Optioner[T, V]): Parsed[V] = {
      val startPos = ctx.success.index

      conv(parse0) match{
        case f: Parsed.Failure =>
          if (ctx.cut) f
          else{
            ctx.cut = false
            ctx.prepareSuccess(optioner.none, startPos)
          }
        case s: Parsed.Success[T] => ctx.prepareSuccess(optioner.some(s.value))
      }
    }

    def |[V >: T](other: => Parsed[V]): Parsed[V] = {
      val startPos = ctx.success.index
      val res = conv(parse0) match {
        case p: Parsed.Success[T] => p
        case f: Parsed.Failure =>
          other match{
            case p: Parsed.Success[V] => p
            case f: Parsed.Failure =>
              f.index = startPos
              f.stack = Nil
              f
          }
      }
      ctx.cut = false
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
    if (ctx.success.index == ctx.input.length) ctx.prepareSuccess(())
    else ctx.prepareFailure(ctx.success.index)
  }

  def Start(implicit ctx: Ctx[_]): Parsed[Unit] = {
    if (ctx.success.index == 0) ctx.prepareSuccess(())
    else ctx.prepareFailure(ctx.success.index)

  }

  def Pass(implicit ctx: Ctx[_]): Parsed[Unit] = ctx.prepareSuccess(())

  def Fail(implicit ctx: Ctx[_]): Parsed[Unit] = ctx.prepareFailure(ctx.success.index)

  def Index(implicit ctx: Ctx[_]): Parsed[Int] = ctx.prepareSuccess(ctx.success.index)

  def AnyChar(implicit ctx: Ctx[_]): Parsed[Unit] = CharPred(_ => true)
  def CharPred(p: Char => Boolean)(implicit ctx: Ctx[_]): Parsed[Unit] = {
    if (!ctx.input.lift(ctx.success.index).exists(p))  ctx.prepareFailure(ctx.success.index)
    else ctx.prepareSuccess((), ctx.success.index + 1)
  }
  def CharsWhile(p: Char => Boolean)(implicit ctx: Ctx[_]) = {
    def currentCharMatches = ctx.input.lift(ctx.success.index).exists(p)
    if (!currentCharMatches)  {
      ctx.isSuccess = false
      ctx.failure
    }else{
      while(currentCharMatches) ctx.success.index += 1
      ctx.prepareSuccess(())
    }
  }


  implicit def parseInputCtx(s: String): Ctx[_] = Ctx(s)
}

class Ctx[+_](val input: String,
              val stack: mutable.Buffer[String],
              val success: Parsed.Success[_],
              val failure: Parsed.Failure,
              var cut: Boolean = false,
              var isSuccess: Boolean){
  def prepareSuccess[T](value: T, index: Int = success.index): Parsed.Success[T] = {
    val res = success.asInstanceOf[Parsed.Success[T]]
    isSuccess = true
    res.value = value
    res.index = index
    res
  }
  def prepareFailure[T](index: Int): Parsed.Failure = {
    isSuccess = false
    failure.stack = Nil
    failure.index = index
    failure
  }
}
object Ctx{
  def apply(input: String) = new Ctx(
    input,
    mutable.Buffer.empty,
    new Parsed.Success,
    new Parsed.Failure,
    cut = false,
    isSuccess = true
  )
}

abstract class Parsed[+T](val isSuccess: Boolean){
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
    var index = 0
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
    var index = 0
    var stack = List.empty[String]
    override def toString() = s"Parsed.Failure($index)"
    def map[V](f: Nothing => V) = this
    def flatMap[V](f: Nothing => Parsed[V]): Parsed[V] = this
    def filter(f: Nothing => Boolean) = this
  }
}