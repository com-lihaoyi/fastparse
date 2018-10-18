package fastparse

import utest._

/**
  * Same as MathTests, but demonstrating the use of whitespace
  */
object WhitespaceTests extends TestSuite{
  val tests = Tests {
    def checkCommon(p: P[Any] => P[Unit]) = {
      val Parsed.Success((), 0) = parse("", p)
      val Parsed.Success((), 0) = parse("/", p)
      val Parsed.Success((), 1) = parse(" /", p)
      val Parsed.Success((), 1) = parse(" / ", p)
      val Parsed.Success((), 2) = parse("//", p)
      val Parsed.Success((), 3) = parse(" //", p)
      val Parsed.Success((), 4) = parse(" // ", p)
      val Parsed.Success((), 13) = parse(" // / / // /*", p)
      val Parsed.Success((), 4) = parse("/**/", p)
      val Parsed.Success((), 5) = parse("/* */", p)
      val Parsed.Success((), 6) = parse("/****/", p)
      val Parsed.Success((), 9) = parse("/** * **/", p)
      val Parsed.Success((), 15) = parse("/** // * // **/", p)
    }
    'scala - {
      checkCommon(ScalaWhitespace.whitespace)
      // allow nested comments
      val Parsed.Failure(_, 11, _) = parse("/** /* /**/", ScalaWhitespace.whitespace)
      val Parsed.Success((), 8) = parse("/*/**/*/", ScalaWhitespace.whitespace)
    }
    'java - {
      checkCommon(JavaWhitespace.whitespace)
      // no nested comments
      val Parsed.Success((), 11) = parse("/** /* /**/", JavaWhitespace.whitespace)
      val Parsed.Success((), 6) = parse("/*/**/*/", JavaWhitespace.whitespace)
    }
    'jsonnet - {
      checkCommon(JsonnetWhitespace.whitespace)
      // no nested comments
      val Parsed.Success((), 11) = parse("/** /* /**/", JsonnetWhitespace.whitespace)
      val Parsed.Success((), 6) = parse("/*/**/*/", JsonnetWhitespace.whitespace)
    }
  }

}