package pythonparse

import utest._

/**
 * Created by haoyi on 10/8/15.
 */
object TestUtils {
  import fasterparser._, Parsing._
  def check[T](rule: P[_] => Parse[T], expected: T, s: String) = {
    implicit def whitespace(cfg: Parse[_]): Parse[Unit] = Pass(cfg)
    def parseIt[_: P] = rule(Parse()) ~ End
    val parsed = Parse(s).read(parseIt(_))
    val stringResult = parsed match {
      case f: Result.Failure => throw new Exception(f.extra.traced.trace)
      case s: Result.Success[T] =>
        val result = s.value
        assert(result == expected)
        result
    }

//    for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
//      val parsed = (rule ~ End).parseIterator(s.grouped(chunkSize))
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
