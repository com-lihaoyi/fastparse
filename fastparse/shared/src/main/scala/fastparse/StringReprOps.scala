package fastparse
import fastparse.utils._
import acyclic.file

object StringReprOps extends ReprOps[Char, String] {
  def apply(input: String, i: Int) = input.charAt(i)
  def slice(input: String, start: Int, end: Int) = input.slice(start, end)
  def length(input: String) = input.length

  def fromArray(input: Array[Char]): String = input.mkString
  def fromSeq(input: Seq[Char]): String = input.mkString
  def fromSingle(input: Char): String = input.toString
  def toArray(input: String): Array[Char] = input.toCharArray
  def flatten(input: Seq[String]): String = input.mkString
  def prettyPrint(input: String): String = input
  def literalize(input: String): String = Utils.literalize(input)
  def errorMessage(input: ParserInput[Char, String], expected: String, idx: Int): String = {
    val locationCode = {
      val first = input.slice(idx - 20, idx)
      val last = input.slice(idx, idx + 20)
      val emptyString = ""
      val lastSnippet: String = last.lines.toSeq.headOption.getOrElse(emptyString)
      val firstSnippet: String = first.reverse.lines.toSeq.headOption.getOrElse(emptyString).reverse

      prettyPrint(firstSnippet) + prettyPrint(lastSnippet) + "\n" + (" " * firstSnippet.length) + "^"
    }
    val literal = literalize(input.slice(idx, idx + 20))
    s"found $literal, expected $expected at index $idx\n$locationCode"
    //TODO Probably we could avoid code duplication by creating only method `locationCode`
    //TODO but it reduces the abstraction
  }

  def prettyIndex(input: ParserInput[Char, String], index: Int): String = {
    input match {
      case IndexedParserInput(data) =>
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
      case _ => String.valueOf(index)
    }
  }

}
