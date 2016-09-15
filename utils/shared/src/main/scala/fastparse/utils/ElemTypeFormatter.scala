package fastparse.utils

abstract class ElemFormatter[Elem, Repr] {
  def prettyPrint(input: Repr): String
  def literalize(input: Repr): String
  def errorMessage(input: ParserInput[Elem, Repr], expected: String, idx: Int): String
  def prettyIndex(input: ParserInput[Elem, Repr], index: Int): String
  def slice0(value: Repr, start: Int, end: Int): Repr
  def apply0(value: Repr, i: Int): Elem
  def length0(value: Repr): Int
}

trait ResultConverter[Elem, ResultType] {
  def convertToRepr(input: IndexedSeq[Elem]): ResultType
  def convertFromRepr(input: ResultType): IndexedSeq[Elem]
}

object ElemFormatter {

  implicit val CharFormatter = new ElemFormatter[Char, String] {
    def apply0(input: String, i: Int) = input.charAt(i)
    def slice0(input: String, start: Int, end: Int) = input.substring(start, end)
    def length0(input: String) = input.length
    override def prettyPrint(input: String): String = input
    override def literalize(input: String): String = Utils.literalize(input)

    override def errorMessage(input: ParserInput[Char, String], expected: String, idx: Int): String = {
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

    override def prettyIndex(input: ParserInput[Char, String], index: Int): String = {
      input match {
        case IndexedParserInput(data) =>
          val lines = data.take(1 + index).lines
          val line = lines.length
          val col = lines.toSeq.lastOption.map(_.length).getOrElse(0)
          s"$line:$col"
        case _ => String.valueOf(index)
      }
    }
  }

}

object ResultConverter {
  implicit val CharBuilder = new ResultConverter[Char, String] {
    override def convertToRepr(input: IndexedSeq[Char]): String = input.mkString
    override def convertFromRepr(input: String): IndexedSeq[Char] = wrapString(input)
  }
}