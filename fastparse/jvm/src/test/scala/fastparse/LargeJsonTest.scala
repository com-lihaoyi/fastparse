package fastparse

import fastparse.JsonTests._
import fastparse.core.Parsed
import utest._
/**
 * Parse the JSON from the parboiled2 testsuite and make
 * sure we can extract meaningful values from it.
 */
object LargeJsonTest extends TestSuite{
  val tests = TestSuite{
    'large{
      val Parsed.Success(value, _) = jsonExpr.parse(
        io.Source.fromInputStream(getClass.getResourceAsStream("/test.json")).mkString
      )
      assert(value(200)("friends")(1)("name").value == "Susan White")
    }
  }
}
