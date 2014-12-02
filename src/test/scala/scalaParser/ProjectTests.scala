package scalaParser

import org.parboiled2.ParseError
import utest._
import utest.framework.Test
import utest.util.Tree

import scala.util.{Failure, Success}

object ProjectTests extends TestSuite{

  println("running")
  def tests = TestSuite{
    def checkFile(path: String) = UnitTests.check(io.Source.fromFile(path).mkString)
    def checkDir(path: String, filter: String => Boolean = _ => false) = {

      def listFiles(s: java.io.File): Iterator[String] = {
        val (dirs, files) = s.listFiles().toIterator.partition(_.isDirectory)
        files.map(_.getPath) ++ dirs.flatMap(listFiles)
      }
      for{
        f <- listFiles(new java.io.File(path))
        if f.endsWith(".scala")
        if !filter(f)
      }{
        println("CHECKING " + f)
        checkFile(f)
      }
    }
    'test - checkFile("src/test/resources/test.scala")

    'scalaParser - checkDir("src")
    'scalaJs - checkDir("scala-js")
    'scalaz - checkDir("scalaz")
    'shapeless - checkDir("shapeless")
    'akka - checkDir("akka")
    'lift - {
      val blacklist = Seq(
        // octal literals are deprecated
        "framework/core/util/src/main/scala/net/liftweb/util/CombParserHelpers.scala"
      )
      checkDir("framework", f => blacklist.exists(f.contains))
    }
    'play - checkDir("playframework", f => f.endsWith(".scala.html"))
    'scala{
      // Things that we won't bother parsing, mainly because they use XML literals
      val blacklist = Seq(
        // Not real Scala files
        "dbuild-meta-json-gen.scala",
        "genprod.scala",
        "disabled", // don't bother parsing disabled tests
        "neg", // or neg tests
        "deprecate-early-type-defs.scala", // or deprecated tests
        // or unicode escapes
        "test/files/run/literals.scala",
        "test/files/run/t3835.scala",
        "test/files/run/richs.scala",
        // Lots of guys in these folders seem
        // to be borked, skip all of them
        "test/files/positions",
        "test/files/presentation",
        "test/pending"
      )
      checkDir("scala", f => blacklist.exists(f.contains))

    }
  }
}