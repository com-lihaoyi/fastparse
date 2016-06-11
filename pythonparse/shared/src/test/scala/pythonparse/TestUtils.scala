package pythonparse

import utest._

/**
 * Created by haoyi on 10/8/15.
 */
object TestUtils {
  import fastparse.allString._
  def check[T](rule: Parser[T], expected: T, s: String) = {
    val parsed = (rule ~ End).parse(s)
    parsed match {
      case f: Parsed.Failure =>
        throw new Exception(f.extra.traced.trace)
      case s: Parsed.Success[T] =>
        val result = s.value
        assert(result == expected)
        result
    }
  }
}
