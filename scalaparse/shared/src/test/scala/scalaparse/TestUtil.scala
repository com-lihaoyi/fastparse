package scalaparse

import fastparse._
import fastparse.core.Parsed
import utest._


import scalaparse.Scala._

/**
 * Created by haoyi on 5/3/15.
 */
object TestUtil {
  def checkNeg[T](input: String, expected: String = "ADA???D", found: String = "ADQW??") = {
//    println("Checking Neg...\n" )
//    println(input)
    Scala.CompilationUnit.parse(input) match{
      case f: Parsed.Failure =>

        val parsedExpected = f.extra.traced.expected
        val parsedFound = input.slice(f.index, f.index + 10)
        val stack = f.extra.traced.trace
        assert(
        { implicitly(input)
          implicitly(stack)
          parsedExpected.trim == expected.trim && parsedFound.startsWith(found)}
        )
      case s: Parsed.Success[_] => assert{implicitly(input); false}
    }
  }

  def check[T](input: String, tag: String = "") = {
//    println("Checking...\n" )
//    println(input)
    val res = Scala.CompilationUnit.parse(input)
    res match{
      case f: Parsed.Failure =>
        //        println(f.formatExpectedAsString)
        //        println(f.formatTraces)
        throw new Exception(tag + "\n" + input + "\n" + f.extra.traced.trace)
      case s: Parsed.Success[_] =>
        //        println(parsed)
        val inputLength = input.length
        assert(s.index == inputLength)
    }
  }
}

