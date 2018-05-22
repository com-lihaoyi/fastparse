package fastparse.utils
import acyclic.file

trait ElemSetHelper[Elem] {
  def toInt(a: Elem): Int
  def ordering: Ordering[Elem]
  def toLowerCase(in: Elem): Elem
  def generateValues(f: Generator.Callback[Elem]): Unit
}
object ElemSetHelper {

  implicit object CharBitSetHelper extends ElemSetHelper[Char] {
    def toInt(a: Char): Int = a
    def ordering = implicitly[Ordering[Char]]
    def toLowerCase(in: Char) = in.toLower
    def generateValues(f: Generator.Callback[Char]) = {
      var i = Char.MinValue.toInt

      while(i <= Char.MaxValue){
        f(i.toChar)
        i = i + 1
      }
    }
  }

}


// Give these concrete classes rather than using anonymous lambdas for debugging
// & profiling purposes, so you can see them clearly in the stack traces
abstract class Generator[Elem]{
  def apply(callback: Generator.Callback[Elem]): Unit
}
object Generator{
  class Iter[Elem](items: Iterator[Elem])extends Generator[Elem]{
    def apply(callback: Generator.Callback[Elem]): Unit = {
      for(i <- items) callback(i)
    }
  }
  class Pred[Elem](predicate: Elem => Boolean)(implicit helper: ElemSetHelper[Elem])
    extends Generator[Elem]{
    def apply(callback: Generator.Callback[Elem]): Unit = helper.generateValues{
      new PredCallback(predicate, callback)
    }
  }
  abstract class Callback[Elem]{
    def apply(v: Elem): Unit
  }
  class PredCallback[Elem](predicate: Elem => Boolean,
                          callback: Callback[Elem]) extends Callback[Elem]{
    def apply(v: Elem): Unit = {
      if(predicate(v)) callback(v)
    }
  }
}
