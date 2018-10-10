package fasterparser

/**
  * The outcome of a [[Parse]] run, either a success (with value and index) or
  * failure (with associated debugging metadata to help figure out what went
  * wrong).
  *
  * Doesn't contain any information not already present in [[Parse]], but
  * packages it up nicely in an immutable case class that's easy for external
  * code to make use of.
  */
sealed abstract class Result[+T](val isSuccess: Boolean){
  def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (T, Int) => V): V
  def get: Result.Success[T]
}

object Result{

  final case class Success[+T](value: T, index: Int) extends Result[T](true){
    def get = this
    def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (T, Int) => V) = onSuccess(value, index)
    override def toString() = s"Result.Success($value, $index)"
  }
  final case class Failure(index: Int,
                     stack: List[(String, Int)],
                     extra: Extra) extends Result[Nothing](false){
    def get = throw new Exception("Parse Error at " + index + ":\n" + stack.mkString("\n"))
    def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (Nothing, Int) => V) = onFailure(index, stack)
    override def toString() = s"Result.Failure($trace)"
    def trace =
      "Expected " +Failure.formatStack(extra.input, stack) +
      ", found " + Failure.formatTrailing(extra.input, stack.head._2)
    def traced = extra.traced

  }
  object Failure{
    def formatStack(input: ParserInput[Char, String], stack: List[(String, Int)]) = {
      stack.reverse.map{case (s, i) => s"$s:${ReprOps.StringReprOps.prettyIndex(input, i)}"}.mkString(" / ")
    }
    def formatTrailing(input: ParserInput[Char, String], index: Int) = {
      Util.literalize(input.slice(index, index + 10))
    }
  }
  case class Extra(input: ParserInput[Char, String],
                   startIndex: Int,
                   index: Int,
                   originalParser: Parse[_] => Parse[_]) {
    def traced: Failure = {
      Parse(input, startIndex = startIndex, traceIndex = index)
        .read[Any](originalParser).asInstanceOf[Failure]
    }
  }
}

