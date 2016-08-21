package scalaparse

import fastparse._
import utest._
import fastparse.all._


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
          expected.trim == parsedExpected.trim && parsedFound.startsWith(found)
        }
        )
      case s: Parsed.Success[_] => assert{implicitly(input); false}
    }
    for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
      val res = Scala.CompilationUnit.parseIterator(input.grouped(chunkSize))
      res match{
        case f: Parsed.Failure =>

          val parsedExpected = f.lastParser.toString
          val parsedFound = input.slice(f.index, f.index + 10)
          // Note, here we check `expected.contains` rather than `expected ==`!
          // This is because when parsing an `Iterator`, the `.extra.traced` that
          // we normally use to get the stack trace doesn't work, so instead we
          // do an approximate check to make sure the parser is somewhere in the
          // expected output. OTOH, the `parsedFound` check can still be the same
          // since that just depends on the `index`
          assert(
            { implicitly(input)
              expected.trim.contains(parsedExpected.trim) && parsedFound.startsWith(found)
            }
          )
        case s: Parsed.Success[_] => assert{implicitly(input); false}
      }
    }
  }

  def check[T](input: String, tag: String = "", skipIterator: Boolean = false) = {
//    println("Checking...\n" )
//    println(input)
    val normalRes = Scala.CompilationUnit.parse(input)
    val iteratorRes =
      if (skipIterator) Nil
      else
        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024))
        yield Scala.CompilationUnit.parseIterator(input.grouped(chunkSize))

    for(res <- normalRes +: iteratorRes){
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
}

