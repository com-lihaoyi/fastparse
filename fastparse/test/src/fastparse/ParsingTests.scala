package test.fastparse
import fastparse._
import utest._

object ParsingTests extends TestSuite{


  import Parsed.{Success, Failure}

  def check[T](parser: P[_] => P[T], input: (String, Int), rhs: Parsed[T]) = {
    val (str, index) = input
    // Test normal parsing
//    val parsed = parse(str, index).read(parser(_))
//    assert(parsed == rhs)
    // Test iterator parsing
    for(chunkSize <- Seq(1)){
      val parsed = parse(str.grouped(chunkSize), parser(_), startIndex = index)
      assert(parsed == rhs)
    }
  }

  def checkFail[T](parser: P[_] => P[T], input: (String, Int), expectedFailureIndex: Int) = {
    val (str, index) = input
    // Test normal parsing
    val parsed = parse(str, parser(_), startIndex = index)
    val failureIndex = parsed.asInstanceOf[Parsed.Failure].index
    assert(failureIndex == expectedFailureIndex)
    // Test iterator parsing
    for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
      val parsed = parse(str.grouped(chunkSize), parser(_), startIndex = index)
      val failureIndex = parsed.asInstanceOf[Parsed.Failure].index
      assert(failureIndex == expectedFailureIndex)
    }

    // Test byte array parsing
    {
      val parsed = parse(str.getBytes, parser(_), startIndex = index)
      val failureIndex = parsed.asInstanceOf[Parsed.Failure].index
      assert(failureIndex == expectedFailureIndex)
    }

    // Test inputStream parsing

    {
      val parsed = parse(new java.io.ByteArrayInputStream(str.getBytes), parser(_), startIndex = index)
      val failureIndex = parsed.asInstanceOf[Parsed.Failure].index
      assert(failureIndex == expectedFailureIndex)
    }
  }

  val tests = Tests {
    import NoWhitespace._

    test("literal"){
      checkFail(implicit c => "Hello WOrld!", ("Hello", 0), 0)
      check(implicit c => "Hello", ("Hello WOrld!", 0), Success((), 5))
      check(implicit c => "H", ("Hello WOrld!", 0), Success((), 1))
      check(implicit c => "Hello".!, ("Hello WOrld!", 0), Success("Hello", 5))
      val variable = "Hello"
      check(implicit c => variable.!, ("Hello WOrld!", 0), Success("Hello", 5))
      checkFail(implicit c => "Hello", ("Hello WOrld!", 5), 5)
      check(implicit c => " WO".!, ("Hello WOrld!", 5), Success(" WO", 8))
    }
    test("literalIgnoreCase"){
      checkFail(implicit c => IgnoreCase("Hello WOrld!"), ("hElLo", 0), 0)
      check(implicit c => IgnoreCase("Hello"), ("hElLo WOrld!", 0), Success((), 5))
      check(implicit c => IgnoreCase("Hello").!, ("hElLo WOrld!", 0), Success("hElLo", 5))
      checkFail(implicit c => IgnoreCase("Hello"), ("hElLo WOrld!", 5), 5)
      check(implicit c => IgnoreCase(" wo").!, ("Hello WOrld!", 5), Success(" WO", 8))
      check(implicit c => IgnoreCase("`~@!3#$4%^&*()-_=+[{]}|\\,.? Hello World"), ("`~@!3#$4%^&*()-_=+[{]}|\\,.? hElLo wOrLd", 0), Success((), 39))
    }
    test("repeat"){
      check(implicit c => "Hello".!.rep, ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check(implicit c => "Hello".!.rep, ("HelloHello!", 2), Success(Seq(), 2))
      check(implicit c => "Hello".!.rep, ("HelloHello!", 5), Success(Seq("Hello"), 10))
      check(implicit c => "Hello".!.rep(1), ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check(implicit c => "Hello".!.rep(1, max = 1), ("HelloHello!", 0), Success(Seq("Hello"), 5))
      check(implicit c => "Hello".!.rep(1, max = 2), ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check(implicit c => "Hello".!.rep(1, max = 2), ("HelloHelloHello!", 0), Success(Seq("Hello", "Hello"), 10))

      check(implicit c => "Hello".!.rep(0, max=0), ("HelloHello!", 0), Success(Seq(), 0))
      // identical :  check( ("Hello" | Pass).!, ("HelloHello!", 0), Success("Hello", 5))
      check(implicit c => "Hello".!.rep(0, max=1), ("HelloHello!", 0), Success(Seq("Hello"), 5))

      checkFail(implicit c => "Hello".rep(1), ("HelloHello!", 2), 2)
      checkFail(implicit c => "Hello".rep ~ "bye" ~ End, ("HelloHello!", 0), 10)
    }
    test("either"){
      check(implicit c => "Hello".! | "Bye".!, ("HelloBye", 0), Success("Hello", 5))
      check(implicit c => ("Hello" | "Bye").!, ("HelloBye", 5), Success("Bye", 8))
      checkFail(implicit c => "Hello" | "Bye", ("HelloBye", 2), 2)
      check(implicit c => ("Hello" | "Bye").!.rep, ("HelloBye", 0), Success(Seq("Hello", "Bye"), 8))
      check(implicit c => ("Hello" | "Bye").rep.!, ("HelloBye", 0), Success("HelloBye", 8))
    }
    test("sequence"){
      def p[_: P] = "Hello".! ~ "Bye".!
      check(implicit c => p, ("HelloBye", 0), Success(("Hello", "Bye"), 8))
      check(implicit c => "Hello".! ~ "Bye".! ~ "!", ("HelloBye!", 0), Success(("Hello", "Bye"), 9))
      check(implicit c => "Hello".! ~ "Bye".! ~ "!".!, ("HelloBye!", 0), Success(("Hello", "Bye", "!"), 9))
      checkFail(implicit c => "Hello" ~ "Bye", ("Bye", 0), 0)
    }
    test("errors"){
      checkFail(implicit c => "Hello" ~ ("omg" | "bbq"), ("Hellookk", 0), 5)
      checkFail(implicit c => "Hello" ~ ("omg" | "bbq"), ("ellookk", 0), 0)
    }
    test("cut"){
      test("local"){
        // Make sure that cuts only apply to enclosing
        test("either"){
          def parser[_: P] = P("hello" | "world" ~ "x" ~/ ("i" | "am" ~ "a") ~ "cow" | "moo")

          // Failing before the cut backtracks all the way out
          val Parsed.Failure(_,0,_) = parse("worldlols", parser(_))
          // Failing after the cut prevents backtracking
          val Parsed.Failure(_,6,_) = parse("worldxlols", parser(_))
          // Failing inside another nested `|` block allows backtracking,
          // but only up to the position of the cut
          val Parsed.Failure(_,6,_) = parse("worldxam", parser(_))
          // Failing *after* the nested `|` block again prevents backtracking
          val Parsed.Failure(_,9,_) = parse("worldxama", parser(_))

          def parser2[_: P] = P("hello" | "world" ~ "x" ~ ("i" | "am" ~/ "a" ~ "b") ~ "a" ~ "cow" | "moo")

          // Failing before the cut backtracks all the way out
          val Parsed.Failure(_,0,_) = parse("worldlols", parser2(_))
          val Parsed.Failure(_,0,_) = parse("worldxlols", parser2(_))
          // Failing inside the nested either's cut prevents backtracking
          val Parsed.Failure(_,8,_) = parse("worldxam", parser2(_))
          val Parsed.Failure(_,9,_) = parse("worldxama", parser2(_))
          // Failing after the nested either with the cut inside fails
          // to backtrack, because the cut has already been crossed
          val Parsed.Failure(_,11,_) = parse("worldxamaba", parser2(_))
        }
        test("optional"){
          def parser[_: P] = P("world" ~ "x" ~/ ("am" ~ "a").? ~ "cow").?

          // Failing before the cut backtracks all the way out
          val Parsed.Success((), 0) = parse("worldlols", parser(_))
          // Failing after the cut prevents backtracking
          val Parsed.Failure(_,6,_) = parse("worldxlols", parser(_))
          // Failing inside another nested `|` block allows backtracking,
          // but only up to the position of the cut
          val Parsed.Failure(_,6,_) = parse("worldxam", parser(_))
          // Failing *after* the nested `|` block again prevents backtracking
          val Parsed.Failure(_,9,_) = parse("worldxama", parser(_))

          def parser2[_: P] = P("world" ~ "x" ~ ("am" ~/ "a" ~ "b").? ~ "a" ~ "cow").?

          // Failing before the cut backtracks all the way out
          val Parsed.Success((), 0) = parse("worldlols", parser2(_))
          val Parsed.Success((), 0) = parse("worldxlols", parser2(_))
          // Failing inside the nested either's cut prevents backtracking
          val Parsed.Failure(_,8,_) = parse("worldxam", parser2(_))
          val Parsed.Failure(_,9,_) = parse("worldxama", parser2(_))
          // Failing after the nested either with the cut inside fails
          // to backtrack, because the cut has already been crossed
          val Parsed.Failure(_,11,_) = parse("worldxamaba", parser2(_))
        }
        test("rep"){
          def parser[_: P] = P("world" ~ "x" ~/ ("am" ~ "a").rep ~ "cow").rep

          // Failing before the cut backtracks all the way out
          val Parsed.Success((), 0) = parse("worldlols", parser(_))
          // Failing after the cut prevents backtracking
          val Parsed.Failure(_,6,_) = parse("worldxlols", parser(_))
          // Failing inside another nested `|` block allows backtracking,
          // but only up to the position of the cut
          val Parsed.Failure(_,6,_) = parse("worldxam", parser(_))
          // Failing *after* the nested `|` block again prevents backtracking
          val Parsed.Failure(_,9,_) = parse("worldxama", parser(_))

          def parser2[_: P] = P("world" ~ "x" ~ ("am" ~/ "a" ~ "b").rep ~ "a" ~ "cow").rep

          // Failing before the cut backtracks all the way out
          val Parsed.Success((), 0) = parse("worldlols", parser2(_))
          val Parsed.Success((), 0) = parse("worldxlols", parser2(_))
          // Failing inside the nested either's cut prevents backtracking
          val Parsed.Failure(_,8,_) = parse("worldxam", parser2(_))
          val Parsed.Failure(_,9,_) = parse("worldxama", parser2(_))
          // Failing after the nested either with the cut inside fails
          // to backtrack, because the cut has already been crossed
          val Parsed.Failure(_,11,_) = parse("worldxamaba", parser2(_))
        }
      }
      test("sequence"){
        check(implicit c => "Hello" ~ ("wtf" ~ "omg" | "wtfom"), ("Hellowtfom", 0), Success((), 10))
        checkFail(implicit c => "Hello" ~ ("wtf" ~ "omg" | "bbq"), ("Hellowtfom", 0), 5)
        checkFail(implicit c => "Hello" ~ ("wtf" ~/ "omg" | "wtfom"), ("Hellowtfom", 0), 8)
        checkFail(implicit c => "Hello" ~ ("wtf" ~ "omg" ~/ "bbq" | "wtfom"), ("Hellowtfomgbbe", 0), 11)
        checkFail(implicit c => "Hello" ~ ("wtf" ~/ "omg" ~ "bbq" | "wtfom"), ("Hellowtfomgbbe", 0), 11)
      }
      test("rep"){
        check(implicit c => ("Hello" ~ "Bye").rep, ("HelloByeHello", 0), Success((), 8))
        checkFail(implicit c => ("Hello" ~/ "Bye").rep, ("HelloByeHello", 0), 13)
        check(implicit c => ("Hello" ~ "Bye").rep, ("HelloByeHello", 0), Success((), 8))
        checkFail(implicit c => "Hello".rep(sep = "Bye" ~/ Pass), ("HelloBye", 0), 8)
        checkFail(implicit c => "Hello".rep(sep = "Bye"./), ("HelloBye", 0), 8)
      }
      test("optional"){
        check(implicit c => ("Hello" ~ "Bye").?, ("HelloBoo", 0), Success((), 0))
        checkFail(implicit c => ("Hello" ~/ "Bye").?, ("HelloBoo", 0), 5)
      }
      test("flatMap"){
        checkFail(implicit c => ("Hello" ~/ "Boo").flatMapX(_ => Fail).?, ("HelloBoo", 0), 8)
        checkFail(implicit c => (("Hello" ~/ "Boo").flatMapX(_ => Pass) ~ Fail).?, ("HelloBoo", 0), 8)
      }
      test("filter"){
        checkFail(implicit c => ("Hello" ~/ "Boo").filter(_ => false) | "", ("HelloBoo", 0), 8)
      }
      test("lookaheadNot"){
        // ! disables cuts: since the whole point of it is to backtrack there
        // isn't any use case where a user would *want* the cuts to take effect
        check(implicit c => !("Hello" ~/ "Bye"), ("HelloBoo", 0), Success((), 0))
        // &() disables cuts: whether it succeeds or fails, the whole point
        // of &() is to backtrack and re-parse things
        check(implicit c => &("Hello" ~/ "Bye") ~ "lol" | "", ("HelloBoo", 0), Success((), 0))
        def p[_: P] = P(  &("Hello" ~/ "Boo") ~ "lol" | "".log("<empty>") )
        check(implicit c => p, ("HelloBoo", 0), Success((), 0))
      }
    }
    test("stringInIgnoreCase"){
      check(implicit c => StringInIgnoreCase("Hello", "Hello World"), ("hElLo WOrld!", 0), Success((), 11))
      check(implicit c => StringInIgnoreCase("abc","abde","abdgh").!, ("ABCDE", 0), Success(("ABC"), 3))
      checkFail(implicit c => StringInIgnoreCase("abc","def","ghi"), ("bcde", 0), 0)
    }
    test("failureMsg"){
      def parser[_: P] = P( "hello" | "world" )
      val f = parse("cow", parser(_)).asInstanceOf[Parsed.Failure]
      val msg = f.trace().msg
      msg ==> """Expected ("hello" | "world"):1:1, found "cow" """.trim
    }
    test("whitespaceFlatMap"){
      // Separated out so they don't pick up the NoWhitespace._ import up top
      checkWhitespaceFlatMap()
      checkNonWhitespaceFlatMap()
    }
  }

  def checkWhitespaceFlatMap() = {
    import fastparse._, SingleLineWhitespace._
    def parser[_: P] = P( CharsWhileIn("a").!.flatMap{n => "b" * n.length} ~ End )
    val Parsed.Success(_, _) = parse("aaa bbb", parser(_))
    val Parsed.Success(_, _) = parse("aa    bb", parser(_))
    val Parsed.Failure(_, _, _) = parse("aaa bb", parser(_))
    val Parsed.Failure(_, _, _) = parse("aaa b", parser(_))
  }

  def checkNonWhitespaceFlatMap() = {
    import fastparse._, SingleLineWhitespace._
    def parser[_: P] = P( CharsWhileIn("a").!.flatMapX{n => "b" * n.length} ~ End )
    val Parsed.Success(_, _) = parse("aaabbb", parser(_))
    val Parsed.Success(_, _) = parse("aabb", parser(_))
    val Parsed.Failure(_, _, _) = parse("aaa bbb", parser(_))
    val Parsed.Failure(_, _, _) = parse("aa bb", parser(_))
  }
}
