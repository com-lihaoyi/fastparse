package cssparse

import fastparse.all._
import utest.asserts.assert

object TestUtil {

  def check[T](input: String, tag: String = "") = {
    val res = CssParser.ruleList.parse(input)
    res match{
      case f: Parsed.Failure =>
        throw new Exception(tag + "\n" + input + "\n" + f.extra.traced.trace)
      case s: Parsed.Success[_] =>
        val inputLength = input.length
        val index = s.index
        assert(index == inputLength)
    }
  }
}
