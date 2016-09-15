package fastparse.utils

abstract class ElemTypeFormatter[ElemType] {
  def prettyPrint(input: IndexedSeq[ElemType]): String
  def literalize(input: IndexedSeq[ElemType]): String
  def errorMessage(input: ParserInput[ElemType], expected: String, idx: Int): String
  def prettyIndex(input: ParserInput[ElemType], index: Int): String
}

trait ResultConverter[ElemType, ResultType] {
  def convertToRepr(input: IndexedSeq[ElemType]): ResultType
  def convertFromRepr(input: ResultType): IndexedSeq[ElemType]
}

object ElemTypeFormatter {

  implicit val CharFormatter = new ElemTypeFormatter[Char] {
    override def prettyPrint(input: IndexedSeq[Char]): String = input.mkString
    override def literalize(input: IndexedSeq[Char]): String = Utils.literalize(input.mkString)

    override def errorMessage(input: ParserInput[Char], expected: String, idx: Int): String = {
      val locationCode = {
        val first = input.slice(idx - 20, idx)
        val last = input.slice(idx, idx + 20)
        val emptyString: IndexedSeq[Char] = ""
        val lastSnippet = Utils.split(last, '\n').headOption.getOrElse(emptyString)
        val firstSnippet = Utils.split(first.reverse, '\n').headOption.getOrElse(emptyString).reverse

        prettyPrint(firstSnippet) + prettyPrint(lastSnippet) + "\n" + (" " * firstSnippet.length) + "^"
      }
      val literal = literalize(input.slice(idx, idx + 20))
      s"found $literal, expected $expected at index $idx\n$locationCode"
      //TODO Probably we could avoid code duplication by creating only method `locationCode`
      //TODO but it reduces the abstraction
    }

    override def prettyIndex(input: ParserInput[Char], index: Int): String = {
      input match {
        case IndexedParserInput(data) =>
          val lines = Utils.split(data.take(1 + index), '\n')
          val line = lines.length
          val col = lines.lastOption.map(_.length).getOrElse(0)
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