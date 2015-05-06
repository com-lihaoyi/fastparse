package scalaparser

import fastparse.Result
import utest._

/**
 * Created by haoyi on 5/3/15.
 */
object TestUtil {
  def checkNeg[T](input: String, expected: String = "ADA???D", found: String = "ADQW??") = {
//    println("Checking Neg...\n" )
//    println(input)
    Scala.CompilationUnit.parse(input) match{
      case f: Result.Failure =>
        val fastparse.Result.Frame(index, parser) = f.stack.last
        val parsedFound = input.slice(index, index + 10)
        val stack = f.trace
        assert(
        {input; stack; parser.toString == expected.trim},
        {input; stack; parsedFound.startsWith(found)}
        )
      case Result.Success(parsed, index, cut) => assert(false)
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
        throw new Exception({input + "\n" + f.toString})
      case s: Result.Success[_] =>
        //        println(parsed)
        val inputLength = input.length
        assert(s.index == inputLength)
    }
  }
}
