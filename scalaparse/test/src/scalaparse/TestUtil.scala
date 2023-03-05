package scalaparse

import utest.assert
import fastparse._



/**
 * Created by haoyi on 5/3/15.
 */
object TestUtil {
  def checkNeg[T](input: String,
                  terminals: String,
                  aggregate: String,
                  found: String)
                 (implicit line: sourcecode.Line) = {
//    println("Checking Neg...\n" )
//    println(input)
    parse(input, Scala.CompilationUnit(_)) match{
      case f: Parsed.Failure =>

        println("=" * 100)
        val trace = f.extra.trace(true)
        val index = f.index
        val parsedTerminals = trace.terminalAggregateString
        val parsedAggregate = trace.groupAggregateString
        val parsedFound = input.slice(f.index, f.index + 10)
        val stack = trace.longAggregateMsg

        assert(
        { implicitly(input)
          implicitly(stack)
          implicitly(index)
          implicitly(parsedFound)
          (aggregate == null || aggregate.trim == parsedAggregate.trim) &&
          (terminals == null || terminals.trim == parsedTerminals.trim) &&
          parsedFound.startsWith(found)
        }
        )

        line.value
      case _: Parsed.Success[_] =>
        assert({implicitly(input); false})
        line.value
    }
//    for(chunkSize <- Seq(/*1, 4, 16, 64, 256, 1024*/)){
//      val res = parse(input.grouped(chunkSize), Scala.CompilationUnit(_))
//      res match{
//        case f: Parsed.Failure =>
//
////          val parsedExpected = f.lastParser.toString
//          val parsedFound = input.slice(f.index, f.index + 10)
//          // Note, here we check `expected.contains` rather than `expected ==`!
//          // This is because when parsing an `Iterator`, the `.extra.traced` that
//          // we normally use to get the stack trace doesn't work, so instead we
//          // do an approximate check to make sure the parser is somewhere in the
//          // expected output. OTOH, the `parsedFound` check can still be the same
//          // since that just depends on the `index`
//          assert(
//            { implicitly(input)
//              /*expected.trim.contains(parsedExpected.trim) && */parsedFound.startsWith(found)
//            }
//          )
//        case s: Parsed.Success[_] => assert{implicitly(input); false}
//      }
//    }
  }

  def check[T](input: String, tag: String = "", skipIterator: Boolean = false) = {
    println("Checking...\n" )
    println(input)
    check0(input, input.length, tag)
    if (!skipIterator) {
      for(chunkSize <- Seq(1, 5, 18, 67, 260, 1029)) {
        println(chunkSize)
        check0(input.grouped(chunkSize), input.length, tag)
      }
    }

    if (!skipIterator) {
      for(chunkSize <- Seq(1, 3, 14, 61, 252, 1019)) {
        check0(ParserInputSource.FromReadable(input, chunkSize), input.length, tag)
      }
    }
  }
  def check0(input: ParserInputSource, inputLength: Int, tag: String) = {
    val res = parse(input, Scala.CompilationUnit(_))
    res match{
      case f: Parsed.Failure =>
        //        println(f.formatExpectedAsString)
        //        println(f.formatTraces)
        println("TRACING")
        println(f)
        throw new Exception(tag + "\n" + input + "\n" + f.trace().msg)
      case s: Parsed.Success[_] =>
        //        println(parsed)
        assert(s.index == inputLength)
    }
  }
}

