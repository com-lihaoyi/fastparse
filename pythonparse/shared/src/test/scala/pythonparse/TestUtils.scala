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
      case f: fastparse.core.Parsed.Failure =>
        throw new Exception(f.extra.traced.trace)
      case s: fastparse.core.Parsed.Success[T] =>
        val result = s.value
        assert(result == expected)
        result
    }
  }
}
