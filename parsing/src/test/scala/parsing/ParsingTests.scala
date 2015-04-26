package parsing
import utest._
import parsing.Parsing._
import parsing.Parsing.Parser._
import parsing.Parsing.Res._
object ParsingTests extends TestSuite{
  val tests = TestSuite{
    'hello{
      assert(
        Literal("Hello WOrld!").parse("Hello", 0) == Failure(0),
        Literal("Hello").parse("Hello WOrld!", 0) == Success("Hello", 5),
        Literal("Hello").parse("Hello WOrld!", 5) == Failure(5),
        Literal(" WO").parse("Hello WOrld!", 5) == Success(" WO", 8)
      )
    }
  }
}
