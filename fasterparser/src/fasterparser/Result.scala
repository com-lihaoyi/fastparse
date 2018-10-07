package fasterparser

abstract class Result[+T](val isSuccess: Boolean){

  def get: Result.Success[T]
}

object Result{
  object Success{
    def unapply[T](x: Result[T]): Option[(T, Int)] = x match{
      case s: Success[T] => Some((s.value, s.index))
      case f: Failure => None
    }
  }
  object Failure{
    def unapply[T](x: Result[T]): Option[(Unit, Int, Unit)] = x match{
      case s: Failure => Some(((), s.index, ()))
      case f: Success[T] => None
    }
  }
  case class Success[+T](value: T, index: Int) extends Result[T](true){
    def get = this

    override def toString() = s"Result.Success($value)"
  }
  case class Failure(index: Int, stack: List[String]) extends Result[Nothing](false){
    def get = throw new Exception("Parse Error at " + index + ":\n" + stack.mkString("\n"))

    override def toString() = s"Result.Failure($index)"
  }
}