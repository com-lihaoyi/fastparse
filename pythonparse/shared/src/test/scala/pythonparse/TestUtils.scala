package pythonparse

import utest._

/**
 * Created by haoyi on 10/8/15.
 */
object TestUtils {
  def check[T](rule: fastparse.core.Parser[T], expected: T, s: String) = {
    import fastparse.all._
    val parsed = (rule ~ End).parse(s)
    parsed match {
      case f: fastparse.core.Result.Failure =>
        throw new Exception(f.traced().trace)
      case s: fastparse.core.Result.Success[T] =>
        val result = s.value
        assert(result == expected)
        result
    }
  }
}
