package pythonparse

import utest._

/**
 * Created by haoyi on 10/8/15.
 */
object TestUtils {
  import fastparse.all._
  def check[T](rule: Parser[T], expected: T, s: String) = {
    val parsed = (rule ~ End).parse(s)
    val stringResult = parsed match {
      case f: Parsed.Failure => throw new Exception(f.extra.traced.trace)
      case s: Parsed.Success[T] =>
        val result = s.value
        assert(result == expected)
        result
    }

    for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
      val parsed = (rule ~ End).parseIterator(s.grouped(chunkSize))
      val stringResult = parsed match {
        case f: Parsed.Failure => throw new Exception(f.extra.traced.trace)
        case s: Parsed.Success[T] =>
          val result = s.value
          assert(result == expected)
      }
    }

    stringResult
  }
}
