package fastparse

import utest._

/**
  * Same as MathTests, but demonstrating the use of whitespace
  */
object WhitespaceTests extends TestSuite{
  val tests = Tests {
    def checkCommon(p0: Whitespace) = {
      val p = p0.apply(_)
      val Parsed.Success((), 0) = parse("", p)
      val Parsed.Success((), 4) = parse(" \t\r\n", p)
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
      val Parsed.Success((), 0) = parse("code", p)
      val Parsed.Success((), 2) = parse("  code", p)
    }
    def checkScriptCommon(p0: Whitespace, delim: Char) = {
      val p = p0.apply(_)
      val Parsed.Success((), 0) = parse("", p)
      val Parsed.Success((), 4) = parse(" \t\r\n", p)
      val Parsed.Success((), 9) = parse(s"$delim comment", p)
      val Parsed.Success((), 10) = parse(s"$delim$delim comment", p)
      val Parsed.Success((), 10) = parse(s" $delim comment", p)
      val Parsed.Success((), 19) = parse(s"$delim comment $delim comment", p)
      val Parsed.Success((), 0) = parse("code", p)
      val Parsed.Success((), 2) = parse("  code", p)
    }
    test("scala"){
      checkCommon(ScalaWhitespace.whitespace)
      // allow nested comments
      val Parsed.Failure(_, 11, _) = parse("/** /* /**/", ScalaWhitespace.whitespace.apply(_))
      val Parsed.Success((), 8) = parse("/*/**/*/", ScalaWhitespace.whitespace.apply(_))
    }
    test("standard_script"){
      checkScriptCommon(ScriptWhitespace.whitespace, '#')
    }
    test("custom_script"){
      checkScriptCommon(ScriptWhitespace.custom('%'), '%')
    }
    test("java"){
      checkCommon(JavaWhitespace.whitespace)
      // no nested comments
      val Parsed.Success((), 11) = parse("/** /* /**/", JavaWhitespace.whitespace.apply(_))
      val Parsed.Success((), 6) = parse("/*/**/*/", JavaWhitespace.whitespace.apply(_))
    }
    test("jsonnet"){
      checkCommon(JsonnetWhitespace.whitespace)
      // no nested comments
      val Parsed.Success((), 11) = parse("/** /* /**/", JsonnetWhitespace.whitespace.apply(_))
      val Parsed.Success((), 6) = parse("/*/**/*/", JsonnetWhitespace.whitespace.apply(_))
    }
  }

}
