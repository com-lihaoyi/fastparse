package fastparse

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
      check("Hello", ("Hello WOrld!", 0), Success.Mutable((), 5))
      check("Hello".!, ("Hello WOrld!", 0), Success.Mutable("Hello", 5))
      checkFail("Hello", ("Hello WOrld!", 5), 5)
      check(" WO".!, ("Hello WOrld!", 5), Success.Mutable(" WO", 8))
    }
    'literalIgnoreCase{
      checkFail(IgnoreCase("Hello WOrld!"), ("hElLo", 0), 0)
      check(IgnoreCase("Hello"), ("hElLo WOrld!", 0), Success.Mutable((), 5))
      check(IgnoreCase("Hello").!, ("hElLo WOrld!", 0), Success.Mutable("hElLo", 5))
      checkFail(IgnoreCase("Hello"), ("hElLo WOrld!", 5), 5)
      check(IgnoreCase(" wo").!, ("Hello WOrld!", 5), Success.Mutable(" WO", 8))
      check(IgnoreCase("`~!@3#$4%^&*()-_=+[{]}|\\,.? Hello World"), ("`~!@3#$4%^&*()-_=+[{]}|\\,.? hElLo wOrLd", 0), Success.Mutable((), 39))
    }
    'repeat{
      check("Hello".!.rep, ("HelloHello!", 0), Success.Mutable(Seq("Hello", "Hello"), 10))
      check("Hello".!.rep, ("HelloHello!", 2), Success.Mutable(Seq(), 2))
      check("Hello".!.rep, ("HelloHello!", 5), Success.Mutable(Seq("Hello"), 10))
      check("Hello".!.rep(1), ("HelloHello!", 0), Success.Mutable(Seq("Hello", "Hello"), 10))
      check(CharIn("abc").!.rep(end=P(&("XYZ"))), ("caXYZaa", 0), Success.Mutable(Seq("c","a"), 2))
      check(CharIn("abc").!.rep(end=P(&("cba"))), ("cacbaaa", 0), Success.Mutable(Seq("c","a"), 2))
      checkFail("Hello".rep(1), ("HelloHello!", 2), 2)
      checkFail("Hello".rep(end="Bye") ~ End, ("HelloHello!", 0), 10)
    }
    'either{
      check("Hello".! | "Bye".!, ("HelloBye", 0), Success.Mutable("Hello", 5))
      check(("Hello" | "Bye").!, ("HelloBye", 5), Success.Mutable("Bye", 8))
      checkFail("Hello" | "Bye", ("HelloBye", 2), 2)
      check(("Hello" | "Bye").!.rep, ("HelloBye", 0), Success.Mutable(Seq("Hello", "Bye"), 8))
      check(("Hello" | "Bye").rep.!, ("HelloBye", 0), Success.Mutable("HelloBye", 8))
    }
    'sequence{
      val p = "Hello".! ~ "Bye".!
      println(p)
      check(p, ("HelloBye", 0), Success.Mutable(("Hello", "Bye"), 8))
      check("Hello".! ~ "Bye".! ~ "!", ("HelloBye!", 0), Success.Mutable(("Hello", "Bye"), 9))
      check("Hello".! ~ "Bye".! ~ "!".!, ("HelloBye!", 0), Success.Mutable(("Hello", "Bye", "!"), 9))
      checkFail("Hello" ~ "Bye", ("Bye", 0), 0)
    }
    'errors{
      checkFail("Hello" ~ ("omg" | "bbq"), ("Hellookk", 0), 5)
      checkFail("Hello" ~ ("omg" | "bbq"), ("ellookk", 0), 0)
    }
    'cut{
      'sequence {
        check("Hello" ~ ("wtf" ~ "omg" | "wtfom"), ("Hellowtfom", 0), Success.Mutable((), 10))
        checkFail("Hello" ~ ("wtf" ~ "omg" | "bbq"), ("Hellowtfom", 0), 5)
        checkFail("Hello" ~ ("wtf" ~! "omg" | "wtfom"), ("Hellowtfom", 0), 8)
        checkFail("Hello" ~ ("wtf" ~ "omg" ~! "bbq" | "wtfom"), ("Hellowtfomgbbe", 0), 11)
        checkFail("Hello" ~ ("wtf" ~! "omg" ~ "bbq" | "wtfom"), ("Hellowtfomgbbe", 0), 11)
      }
      'rep {
        check(("Hello" ~ "Bye").rep, ("HelloByeHello", 0), Success.Mutable((), 8))
        checkFail(("Hello" ~! "Bye").rep, ("HelloByeHello", 0), 13)
        check(("Hello" ~ "Bye").rep, ("HelloByeHello", 0), Success.Mutable((), 8))
        checkFail("Hello".rep(sep = "Bye" ~! Pass), ("HelloBye", 0), 8)
      }
      'optional{
        check(("Hello" ~ "Bye").?, ("HelloBoo", 0), Success.Mutable((), 0))
        checkFail(("Hello" ~! "Bye").?, ("HelloBoo", 0), 5)
      }
    }
  }
}

