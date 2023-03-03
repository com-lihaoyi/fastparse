package pythonparse

import utest._

/**
 * Created by haoyi on 10/8/15.
 */
object TestUtils {
  import fastparse._
  def check[T](rule: P[_] => P[T], expected: T, s: String) = {
    import fastparse.NoWhitespace._
    def parseIt[$: P] = rule(P.current) ~ End
    val parsed = parse(s, parseIt(_))
    val stringResult = parsed match {
      case f: Parsed.Failure => throw new Exception(f.trace().longTerminalsMsg)
      case s: Parsed.Success[T] =>
        val result = s.value
        assert(result == expected)
        result
    }

//    for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
//      val parsed = (rule ~ End).parse(s.grouped(chunkSize))
//      val stringResult = parsed match {
//        case f: Result.Failure => throw new Exception(f.extra.traced.trace)
//        case s: Result.Success[T] =>
//          val result = s.value
//          assert(result == expected)
//      }
//    }

    stringResult
  }
}
