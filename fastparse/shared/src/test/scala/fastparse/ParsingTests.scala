package fastparse
import all._
import utest._

object ParsingTests extends TestSuite{


  import Parsed.{Success, Failure}

  def check[T](parser: P[T], input: (String, Int), rhs: Parsed[T]) = {
    val (str, index) = input
    // Test normal parsing
    val parsed = parser.parse(str, index)
    assert(parsed == rhs)
    // Test iterator parsing
    for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
      val parsed = parser.parse(str, index)
      assert(parsed == rhs)
    }
  }
  def checkFail[T](parser: P[T], input: (String, Int), expectedFailureIndex: Int) = {
    val (str, index) = input
    // Test normal parsing
    val parsed = parser.parse(str, index)
    val failureIndex = parsed.asInstanceOf[Parsed.Failure].index
    assert(failureIndex == expectedFailureIndex)
    // Test iterator parsing
    for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
      val parsed = parser.parseIterator(str.grouped(chunkSize), index)
      val failureIndex = parsed.asInstanceOf[Parsed.Failure].index
      assert(failureIndex == expectedFailureIndex)
    }

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
      check(IgnoreCase("`~@!3#$4%^&*()-_=+[{]}|\\,.? Hello World"), ("`~@!3#$4%^&*()-_=+[{]}|\\,.? hElLo wOrLd", 0), Success((), 39))
    }
    'repeat{
      check("Hello".!.rep, ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".!.rep, ("HelloHello!", 2), Success(Seq(), 2))
      check("Hello".!.rep, ("HelloHello!", 5), Success(Seq("Hello"), 10))
      check("Hello".!.rep(1), ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".!.rep(1, max = 1), ("HelloHello!", 0), Success(Seq("Hello"), 5))
      check("Hello".!.rep(1, max = 2), ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".!.rep(1, max = 2), ("HelloHelloHello!", 0), Success(Seq("Hello", "Hello"), 10))

      check("Hello".!.rep(0, max=0), ("HelloHello!", 0), Success(Seq(), 0))
      // identical :  check( ("Hello" | Pass).!, ("HelloHello!", 0), Success("Hello", 5))
      check("Hello".!.rep(0, max=1), ("HelloHello!", 0), Success(Seq("Hello"), 5))

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
      'flatMap{
        checkFlatmap()
      }
      'filter{
        checkFail(("Hello" ~/ "Boo").filter(_ => false) | "", ("HelloBoo", 0), 0)
      }
      'lookaheadNot{
        // ! disables cuts: since the whole point of it is to backtrack there
        // isn't any use case where a user would *want* the cuts to take effect
        check(!("Hello" ~/ "Bye"), ("HelloBoo", 0), Success((), 0))
        // &() disables cuts: whether it succeeds or fails, the whole point
        // of &() is to backtrack and re-parse things
        check(&("Hello" ~/ "Bye") ~ "lol" | "", ("HelloBoo", 0), Success((), 0))
        check(&("Hello" ~/ "Boo") ~ "lol" | "", ("HelloBoo", 0), Success((), 0))
      }
    }
    'stringInIgnoreCase {
       check(StringInIgnoreCase("Hello", "Hello World"), ("hElLo WOrld!", 0), Success((), 11))
       check(StringInIgnoreCase("abc","abde","abdgh").!, ("ABCDE", 0), Success(("ABC"), 3))
       checkFail(StringInIgnoreCase("abc","def","ghi"), ("bcde", 0), 0)
    }
  }
  // Broken out of the TestSuite block to avoid problems in our 2.10.x
  // build due to https://issues.scala-lang.org/browse/SI-7987
  def checkFlatmap() = {
    checkFail(("Hello" ~/ "Boo").flatMap(_ => Fail).?, ("HelloBoo", 0), 8)
    checkFail((("Hello" ~/ "Boo").flatMap(_ => Pass) ~ Fail).?, ("HelloBoo", 0), 8)
  }
}

