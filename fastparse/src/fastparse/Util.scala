package fastparse

import java.util

import scala.annotation.{switch, tailrec}
import scala.collection.mutable.ArrayBuffer

object Util {

  def startsWithIgnoreCase(src: ParserInput, prefix: IndexedSeq[Char], offset: Int) = {
    @tailrec def rec(i: Int): Boolean = {
      if (i >= prefix.length) true
      else if(!src.isReachable(i + offset)) false
      else {
        val c1: Char = src(i + offset)
        val c2: Char = prefix(i)
        if (c1 != c2 && c1.toLower != c2.toLower) false
        else rec(i + 1)
      }
    }
    rec(0)
  }
  def lineNumberLookup(data: String): Array[Int] = {
    val lineStarts = new ArrayBuffer[Int]()
    var i = 0
    var col = 1
    var cr = false
    while (i < data.length){
      if (data(i) == '\r') {
        col = 1
        cr = true
      }else if (data(i) == '\n') {
        col = 1
        cr = false
      }else{
        if (col == 1) lineStarts.append(i)
        col += 1
        cr = false
      }
      i += 1
    }

    lineStarts.toArray
  }

  def literalize(s: IndexedSeq[Char], unicode: Boolean = false) = {
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
}
object Lazy{
  implicit def make[T](calc0: () => T) = new Lazy(calc0)
}
class Lazy[T](calc0: () => T){
  lazy val force = calc0()
  def apply(): T = force
}

case class Logger(f: String => Unit)
object Logger {
  implicit val stdout = Logger(println)
}

trait Instrument{
  def beforeParse(parser: String, index: Int): Unit
  def afterParse(parser: String, index: Int, success: Boolean): Unit
}