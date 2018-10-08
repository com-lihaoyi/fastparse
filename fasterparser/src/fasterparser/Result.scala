package fasterparser

abstract class Result[+T](val isSuccess: Boolean){
  def fold[V](onFailure: (Int, List[String]) => V, onSuccess: (T, Int) => V): V
  def get: Result.Success[T]
}

object Result{
//  object Success{
//    def unapply[T](x: Result[T]): Option[(T, Int)] = x match{
//      case s: Success[T] => Some((s.value, s.index))
//      case f: Failure => None
//    }
//  }
//  object Failure{
//    def unapply[T](x: Result[T]): Option[(Unit, Int, Unit)] = x match{
//      case s: Failure => Some(((), s.index, ()))
//      case f: Success[T] => None
//    }
//  }
  case class Success[+T](value: T, index: Int) extends Result[T](true){
    def get = this
    def fold[V](onFailure: (Int, List[String]) => V, onSuccess: (T, Int) => V) = onSuccess(value, index)
    override def toString() = s"Result.Success($value)"
  }
  case class Failure(index: Int, stack: List[String]) extends Result[Nothing](false){
    def get = throw new Exception("Parse Error at " + index + ":\n" + stack.mkString("\n"))
    def fold[V](onFailure: (Int, List[String]) => V, onSuccess: (Nothing, Int) => V) = onFailure(index, stack)
    override def toString() = s"Result.Failure($index)"
  }
}