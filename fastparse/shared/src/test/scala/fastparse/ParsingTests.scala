package fastparse
import all._
import fastparse.core.Result
import utest._

object ParsingTests extends TestSuite{


  import Result.{Success, Failure}

  def check[T](parser: P[T], input: (String, Int), rhs: Result[T]) = {
    val (str, index) = input
    val parsed = parser.parse(str, index)
    assert({parser; str; parsed} == rhs)
  }
  def checkFail[T](parser: P[T], input: (String, Int), expectedFailureIndex: Int) = {
    val (str, index) = input
    val parsed = parser.parse(str, index)
    val failureIndex = parsed.asInstanceOf[Failure].index
    assert({parser; str; failureIndex} == expectedFailureIndex)
  }
  val tests = TestSuite{


    'literal{
      checkFail("Hello WOrld!", ("Hello", 0), 0)
      check("Hello", ("Hello WOrld!", 0), Success((), 5))
      check("Hello".!, ("Hello WOrld!", 0), Success("Hello", 5))
      checkFail("Hello", ("Hello WOrld!", 5), 5)
      check(" WO".!, ("Hello WOrld!", 5), Success(" WO", 8))
    }
    'literalIgnoreCase{
      checkFail(IgnoreCase("Hello WOrld!"), ("hElLo", 0), 0)
      check(IgnoreCase("Hello"), ("hElLo WOrld!", 0), Success((), 5))
      check(IgnoreCase("Hello").!, ("hElLo WOrld!", 0), Success("hElLo", 5))
      checkFail(IgnoreCase("Hello"), ("hElLo WOrld!", 5), 5)
      check(IgnoreCase(" wo").!, ("Hello WOrld!", 5), Success(" WO", 8))
      check(IgnoreCase("`~!@3#$4%^&*()-_=+[{]}|\\,.? Hello World"), ("`~!@3#$4%^&*()-_=+[{]}|\\,.? hElLo wOrLd", 0), Success((), 39))
    }
    'repeat{
      check("Hello".!.rep, ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".!.rep, ("HelloHello!", 2), Success(Seq(), 2))
      check("Hello".!.rep, ("HelloHello!", 5), Success(Seq("Hello"), 10))
      check("Hello".!.rep(1), ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".!.rep(1, max = 1), ("HelloHello!", 0), Success(Seq("Hello"), 5))
      check("Hello".!.rep(1, max = 2), ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".!.rep(1, max = 2), ("HelloHelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      checkFail("Hello".rep(1), ("HelloHello!", 2), 2)
      checkFail("Hello".rep ~ "bye" ~ End, ("HelloHello!", 0), 10)
    }
    'either{
      check("Hello".! | "Bye".!, ("HelloBye", 0), Success("Hello", 5))
      check(("Hello" | "Bye").!, ("HelloBye", 5), Success("Bye", 8))
      checkFail("Hello" | "Bye", ("HelloBye", 2), 2)
      check(("Hello" | "Bye").!.rep, ("HelloBye", 0), Success(Seq("Hello", "Bye"), 8))
      check(("Hello" | "Bye").rep.!, ("HelloBye", 0), Success("HelloBye", 8))
    }
    'sequence{
      val p = "Hello".! ~ "Bye".!
      println(p)
      check(p, ("HelloBye", 0), Success(("Hello", "Bye"), 8))
      check("Hello".! ~ "Bye".! ~ "!", ("HelloBye!", 0), Success(("Hello", "Bye"), 9))
      check("Hello".! ~ "Bye".! ~ "!".!, ("HelloBye!", 0), Success(("Hello", "Bye", "!"), 9))
      checkFail("Hello" ~ "Bye", ("Bye", 0), 0)
    }
    'errors{
      checkFail("Hello" ~ ("omg" | "bbq"), ("Hellookk", 0), 5)
      checkFail("Hello" ~ ("omg" | "bbq"), ("ellookk", 0), 0)
    }
    'cut{
      'sequence {
        check("Hello" ~ ("wtf" ~ "omg" | "wtfom"), ("Hellowtfom", 0), Success((), 10))
        checkFail("Hello" ~ ("wtf" ~ "omg" | "bbq"), ("Hellowtfom", 0), 5)
        checkFail("Hello" ~ ("wtf" ~/ "omg" | "wtfom"), ("Hellowtfom", 0), 8)
        checkFail("Hello" ~ ("wtf" ~ "omg" ~/ "bbq" | "wtfom"), ("Hellowtfomgbbe", 0), 11)
        checkFail("Hello" ~ ("wtf" ~/ "omg" ~ "bbq" | "wtfom"), ("Hellowtfomgbbe", 0), 11)
      }
      'rep {
        check(("Hello" ~ "Bye").rep, ("HelloByeHello", 0), Success((), 8))
        checkFail(("Hello" ~/ "Bye").rep, ("HelloByeHello", 0), 13)
        check(("Hello" ~ "Bye").rep, ("HelloByeHello", 0), Success((), 8))
        checkFail("Hello".rep(sep = "Bye" ~/ Pass), ("HelloBye", 0), 8)
      }
      'optional{
        check(("Hello" ~ "Bye").?, ("HelloBoo", 0), Success((), 0))
        checkFail(("Hello" ~/ "Bye").?, ("HelloBoo", 0), 5)
      }
    }
  }
}

