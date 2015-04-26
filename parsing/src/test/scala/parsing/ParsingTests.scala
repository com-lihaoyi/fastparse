package parsing
import utest._
import Parsing._
object ParsingTests extends TestSuite{
  val tests = TestSuite{
    'hello{
      rule("Hello WOrld!")
    }
  }
}
