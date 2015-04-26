package parsing
import utest._
import parsing.Parsing._
import parsing.Parsing.Parser._
import parsing.Parsing.Res._
object ParsingTests extends TestSuite{
  def check[T](parser: Parser[T], input: (String, Int), rhs: Res[T]) = {
    val (str, index) = input
    val parsed = parser.parse(str, index)
    assert({parser; str; parsed} == rhs)
  }
  val tests = TestSuite{

    'literal{
      check("Hello WOrld!", ("Hello", 0), Failure(0))
      check("Hello", ("Hello WOrld!", 0), Success("Hello", 5))
      check("Hello", ("Hello WOrld!", 5), Failure(5))
      check(" WO", ("Hello WOrld!", 5), Success(" WO", 8))
    }
    'repeat{
      check("Hello".rep, ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".rep, ("HelloHello!", 2), Success(Seq(), 2))
      check("Hello".rep, ("HelloHello!", 5), Success(Seq("Hello"), 10))

      check("Hello".rep1, ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".rep1, ("HelloHello!", 2), Failure(2))
    }
    'either{
      check("Hello" | "Bye", ("HelloBye", 0), Success("Hello", 5))
      check("Hello" | "Bye", ("HelloBye", 5), Success("Bye", 8))
      check("Hello" | "Bye", ("HelloBye", 2), Failure(2))
      check(("Hello" | "Bye").rep, ("HelloBye", 0), Success(Seq("Hello", "Bye"), 8))
    }
  }
}
