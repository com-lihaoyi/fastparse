package scalaparse

import fastparse._
import fastparse.core.Result
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
      case f: Result.Failure =>
        val Result.Frame(index, parser) = f.stack.last
        val parsedFound = input.slice(index, index + 10)
        val stack = f.trace
        assert(
        { implicitly(input);
          implicitly(stack);
          parser.toString == expected.trim && parsedFound.startsWith(found)}
        )
      case s: Result.Success[_] => assert{implicitly(input); false}
    }
  }

  def check[T](input: String, tag: String = "") = {
//    println("Checking...\n" )
//    println(input)
    val res = Scala.CompilationUnit.parse(input)
    res match{
      case f: Result.Failure =>
        //        println(f.formatExpectedAsString)
        //        println(f.formatTraces)
        throw new Exception(tag + "\n" + input + "\n" + f.toString)
      case s: Result.Success[_] =>
        //        println(parsed)
        val inputLength = input.length
        assert(s.index == inputLength)
    }
  }
}
