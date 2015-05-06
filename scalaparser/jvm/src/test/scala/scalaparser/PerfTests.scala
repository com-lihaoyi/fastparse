package scalaparser

import utest._

import scala.tools.nsc.{Global, Settings}

object PerfTests extends TestSuite{
  val tests = TestSuite{
    'GenJSCode{

      // Last measurements, runs in 30s:
      // parboiled2: 446 443 447
      //
      // Initial parsing: 104 123 122
      // Parboiled2 is 3.9 times faster
      //
      // Current parsing: 373 371 422
      val input = scala.io.Source.fromFile(
        "target/repos/scala-js/compiler/src/main/scala/org/scalajs/core/compiler/GenJSCode.scala"
      ).mkString

      println("Loaded " + input.length + " bytes of input. Parsing...")
      val start = System.currentTimeMillis()
      var count = 0
      while(System.currentTimeMillis() - start < 30000){
        Scala.CompilationUnit.parse(input, trace = false)
        //        global.newUnitParser(input).parse()
        count += 1
      }
      count

    }
  }
}
