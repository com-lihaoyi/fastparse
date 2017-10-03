package fasterparser

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

    if (ctx.input.startsWith(s, ctx.position)) {
      ctx.position = ctx.position + s.length
      ctx.isSuccess = true
      val res = ctx.success.asInstanceOf[Parsed.Success[Unit]]
      res.value = ()
      res
    }else{
      ctx.isSuccess = false
      ctx.failure.index = ctx.position
      ctx.failure.stack = Nil

      ctx.failure
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
            case p: Parsed.Success[V] =>
              val r = p.asInstanceOf[Parsed.Success[R]]
              val applied = s.apply(pValue, p.value)
              r.value = applied
              r

          }
      }
    }
  }

  implicit class ByNameOps[S, T](parse0: => S)(implicit conv: S => Parsed[T], ctx: Ctx[_]){
    def rep[V](implicit repeater: Implicits.Repeater[T, V]): Parsed[V] = rep(sep=null)
    def rep[V](sep: => Parsed[_] = null)(implicit repeater: Implicits.Repeater[T, V]): Parsed[V] = {
      val acc = repeater.initial
      def end() = {
        val res = ctx.success.asInstanceOf[Parsed.Success[V]]
        res.value = repeater.result(acc)
        res
      }
      def rec(): Parsed[V] = {
        ctx.cut = false
        conv(parse0) match{
          case f: Parsed.Failure =>
            ctx.cut = false
            if (ctx.cut) ctx.failure
            else end()

          case s: Parsed.Success[T] =>
            ctx.cut = false
            repeater.accumulate(s.value, acc)

            sep match{
              case null => rec()
              case sepRes => sepRes match{
                case s: Parsed.Success[_] => rec()
                case f: Parsed.Failure =>
                  if (ctx.cut) ctx.failure
                  else end()
              }
            }
        }
      }
      val res = rec()
      ctx.cut = false
      res
    }

    def log()(implicit name: sourcecode.Name): Parsed[T] = {
      println("Starting " + name.value + " " + ctx.position)
      val res = conv(parse0)
      println("Ending   " + name.value + " " + ctx.position + " " + ctx.isSuccess)
      res
    }

    def ! : Parsed[String] = {
      val startPos = ctx.position

      conv(parse0) match{
        case f: Parsed.Failure => f
        case s: Parsed.Success[_] =>
          val endPos= ctx.position
          val ret = s.asInstanceOf[Parsed.Success[String]]
          ret.value = ctx.input.substring(startPos, endPos)
          ret
      }
    }

    def ?[V](implicit optioner: Implicits.Optioner[T, V]): Parsed[V] = {
      val startPos = ctx.position

      val success = ctx.success.asInstanceOf[Parsed.Success[V]]
      conv(parse0) match{
        case f: Parsed.Failure =>
          if (ctx.cut) f
          else{
            success.value = optioner.none
            ctx.position = startPos
            ctx.cut = false
            success
          }
        case s: Parsed.Success[T] =>
          success.value = optioner.some(s.value)
          success
      }
    }

    def |[V >: T](other: => Parsed[V]): Parsed[V] = {
      val startPos = ctx.position
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
  def CharsWhileIn(s: String)(implicit ctx: Ctx[_]) = CharsWhile(s.toSet)

  def CharIn(s: CharSequence*)(implicit ctx: Ctx[_]) = {
    val set = s.flatMap(_.toString).toSet
    def currentCharMatches = ctx.input.lift(ctx.position).exists(set)
    if (!currentCharMatches)  {
      ctx.isSuccess = false
      ctx.failure
    }else{
      ctx.position += 1
      val res = ctx.success.asInstanceOf[Parsed.Success[Unit]]
      res.value = ()
      res
    }
  }
  def CharsWhile(p: Char => Boolean)(implicit ctx: Ctx[_]) = {
    def currentCharMatches = ctx.input.lift(ctx.position).exists(p)
    if (!currentCharMatches)  {
      ctx.isSuccess = false
      ctx.failure
    }else{
      while(currentCharMatches) ctx.position += 1

      val res = ctx.success.asInstanceOf[Parsed.Success[Unit]]
      res.value = ()
      res
    }

  }


  implicit def parseInputCtx(s: String): Ctx[_] = Ctx(s)
}

class Ctx[+_](val input: String,
              var position: Int,
              val stack: mutable.Buffer[String],
              val success: Parsed.Success[_],
              val failure: Parsed.Failure,
              var cut: Boolean = false,
              var isSuccess: Boolean)
object Ctx{
  def apply(input: String) = new Ctx(
    input,
    0,
    mutable.Buffer.empty,
    new Parsed.Success,
    new Parsed.Failure,
    cut = false,
    isSuccess = true
  )
}

abstract class Parsed[+T](val isSuccess: Boolean){
  def map[V](f: T => V): Parsed[V]
}

object Parsed{
  class Success[T] extends Parsed[T](true){
    var value: T = null.asInstanceOf[T]
    def map[V](f: T => V) = {
      val this2 = this.asInstanceOf[Success[V]]
      this2.value = f(value)
      this2
    }
    override def toString() = s"Parsed.Success($value)"
  }
  class Failure extends Parsed[Nothing](false){
    var index = 0
    var stack = List.empty[String]
    override def toString() = s"Parsed.Failure($index)"
    def map[V](f: Nothing => V) = this
  }
}