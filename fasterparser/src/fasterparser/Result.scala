package fasterparser

abstract class Result[+T](val isSuccess: Boolean){
  def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (T, Int) => V): V
  def get: Result.Success[T]
}

object Result{

  case class Success[+T](value: T, index: Int) extends Result[T](true){
    def get = this
    def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (T, Int) => V) = onSuccess(value, index)
    override def toString() = s"Result.Success($value)"
  }
  case class Failure(index: Int,
                     stack: List[(String, Int)],
                     extra: Extra) extends Result[Nothing](false){
    def get = throw new Exception("Parse Error at " + index + ":\n" + stack.mkString("\n"))
    def fold[V](onFailure: (Int, List[(String, Int)]) => V, onSuccess: (Nothing, Int) => V) = onFailure(index, stack)
    override def toString() = s"Result.Failure(${Failure.formatStack(extra.input, stack)})"
  }
  object Failure{
    def formatStack(input: String, stack: List[(String, Int)]) = {
      stack.map{case (s, i) => s"$s:${Util.prettyIndex(input, i)}"}.mkString(" ")
    }
  }
  case class Extra(input: String,
                   startIndex: Int){
  }
}
