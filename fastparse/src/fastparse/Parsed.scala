package fastparse

/**
  * The outcome of a [[ParsingRun]] run, either a success (with value and index) or
  * failure (with associated debugging metadata to help figure out what went
  * wrong).
  *
  * Doesn't contain any information not already present in [[ParsingRun]], but
  * packages it up nicely in an immutable case class that's easy for external
  * code to make use of.
  */
sealed abstract class Parsed[+T](val isSuccess: Boolean){
  def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (T, Int) => V): V
  def get: Parsed.Success[T]
}

object Parsed{

  final case class Success[+T](value: T, index: Int) extends Parsed[T](true){
    def get = this
    def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (T, Int) => V) = onSuccess(value, index)
    override def toString() = s"Parsed.Success($value, $index)"
  }
  final case class Failure(stack: List[(String, Int)],
                           index: Int,
                           extra: Extra) extends Parsed[Nothing](false){
    def get = throw new Exception("Parse Error at " + index + ":\n" + stack.mkString("\n"))
    def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (Nothing, Int) => V) = onFailure(index, stack)
    override def toString() = s"Parsed.Failure($trace)"
    def trace = {
      stack match{
        case List(("", index)) =>
          "Position " + extra.input.prettyIndex(index) +
            ", found " + Failure.formatTrailing(extra.input, stack.head._2)
        case s =>
          "Expected " + Failure.formatStack(extra.input, stack) +
            ", found " + Failure.formatTrailing(extra.input, stack.head._2)
      }

    }
    def traced = extra.traced

  }
  object Failure{
    def formatStack(input: ParserInput, stack: List[(String, Int)]) = {
      stack.reverse.map{case (s, i) => s"$s:${input.prettyIndex(i)}"}.mkString(" / ")
    }
    def formatTrailing(input: ParserInput, index: Int) = {
      Util.literalize(input.slice(index, index + 10))
    }
  }
  case class Extra(input: ParserInput,
                   startIndex: Int,
                   index: Int,
                   originalParser: ParsingRun[_] => ParsingRun[_]) {
    def traced: Failure = {
      input.checkTraceable()
      parseInput[Any](input, originalParser, startIndex = startIndex, traceIndex = index)
        .asInstanceOf[Failure]
    }
  }
}

