package fastparse.internal

import fastparse.{ParserInput, ParsingRun}

import scala.annotation.{switch, tailrec}
import scala.collection.mutable.ArrayBuffer

object Util {
  def parenthize(fs: Seq[Lazy[String]]) = fs.reverseIterator.map(_()).toSeq.distinct match{
    case Seq(x) => x
    case xs => xs.mkString("(", " | ", ")")
  }
  def joinBinOp(lhs: Msgs, rhs: Msgs) =
    if (lhs.value.isEmpty) rhs
    else if (rhs.value.isEmpty) lhs
    else Msgs(List(new Lazy(() => lhs.render + " ~ " + rhs.render)))

  def consumeWhitespace[V](whitespace: ParsingRun[_] => ParsingRun[Unit], ctx: ParsingRun[Any]) = {
    val oldCapturing = ctx.noDropBuffer // completely disallow dropBuffer
    ctx.noDropBuffer = true
    whitespace(ctx)
    ctx.noDropBuffer = oldCapturing
  }

  def startsWith(src: ParserInput, prefix: String, offset: Int) = {
    @tailrec def rec(i: Int): Boolean = {
      if (i >= prefix.length) true
      else if (!src.isReachable(i + offset)) false
      else if (src(i + offset) != prefix.charAt(i)) false
      else rec(i + 1)
    }
    rec(0)
  }

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
    var prev: Character = null
    while (i < data.length){
      val char = data(i)
      if (char == '\r') {
        if (prev != '\n' && col == 1) lineStarts.append(i)
        col = 1
        cr = true
      }else if (char == '\n') {
        if (prev != '\r' && col == 1) lineStarts.append(i)
        col = 1
        cr = false
      }else{
        if (col == 1) lineStarts.append(i)
        col += 1
        cr = false
      }
      prev = char
      i += 1
    }
    if (col == 1) lineStarts.append(i)

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

class Lazy[T](calc0: () => T){
  lazy val force = calc0()
  def apply(): T = force
}

case class Logger(f: String => Unit)
object Logger {
  implicit val stdout: Logger = Logger(println)
}

trait Instrument{
  def beforeParse(parser: String, index: Int): Unit
  def afterParse(parser: String, index: Int, success: Boolean): Unit
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
final class CompactTrieNode(source: TrieNode){
  val children: Map[Char, (String, CompactTrieNode)] = {
    val iter = for(char <- source.children.keys) yield char -> {
      val string = new StringBuilder
      var child = source.children(char)
      while(!child.word && child.children.size == 1){
        string.append(child.children.keys.head)
        child = child.children.values.head
      }
      (string.toString(), new CompactTrieNode(child))
    }
    iter.toMap
  }


  val word = source.word
}
object Msgs{
  val empty = Msgs(Nil)
}
case class Msgs(value: List[Lazy[String]]){
  def :::(other: Msgs) = Msgs(other.value ::: value)
  def ::(other: Lazy[String]) = Msgs(other :: value)
  override def toString = render
  def render = Util.parenthize(value)
}