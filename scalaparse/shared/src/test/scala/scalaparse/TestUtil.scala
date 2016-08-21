package scalaparse

import fastparse._
import utest._
import fastparse.all._


import scalaparse.Scala._


object TestUtil {

  def checkNeg0(input: String,
                expected: String,
                found: String,
                makeInput: String => ParserInput[Char]) = {
//    println("Checking Neg...\n" )
//    println(input)
    Scala.CompilationUnit.parseInput(makeInput(input)) match{
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

  def check0(input: String,
             tag: String = "",
             makeInput: String => ParserInput[Char]) = {
//    println("Checking...\n" )
//    println(input)
    val res = Scala.CompilationUnit.parseInput(makeInput(input))
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

