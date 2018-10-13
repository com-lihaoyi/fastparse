package json

import scalaparse.ScalacParser
import utest._

import scala.tools.nsc.{Global, Settings}

object ScalaBench extends TestSuite{
  def bench(f: => Unit) = {
    var start = System.currentTimeMillis()
    while(System.currentTimeMillis() - start < 10000) {
      f
    }
    start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < 10000) {
      f
      count += 1
    }
    count
  }
  val txt = scala.io.Source
    .fromFile("perftests/resources/GenJSCode.scala")
    .mkString

  val tests = Tests{
    'fastparse - bench{

      fastparse.parse(txt, scalaparse.Scala.CompilationUnit(_))
    }
    'scalac - bench{
      ScalacParser.checkParseFails(txt)
    }

  }
}