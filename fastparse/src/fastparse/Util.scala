package fastparse

import scala.annotation.switch

object Util {
  def prettyIndex(data: String, index: Int): String = {
    var line = 1
    var col = 1
    var i = 0
    while (i < index){
      if (data(i) == '\n') {
        col = 1
        line += 1
      }else{
        col += 1
      }
      i += 1
    }
    s"$line:$col"
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

case class Logger(f: String => Unit)
object Logger {
  implicit val stdout = Logger(println)
}