package scalaparse

import java.nio.file.{Paths, Path, Files}
import concurrent.ExecutionContext.Implicits.global
import utest._
import utest.framework.Test
import utest.util.Tree

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import TestUtil._

object ProjectTests extends TestSuite{

  println("running")
  def tests = TestSuite{

    def checkDir(path: String, filter: String => Boolean = _ => true) = {

      val failures = mutable.Buffer.empty[String]
      def listFiles(s: java.io.File): Iterator[String] = {
        val (dirs, files) = Option(s.listFiles()).toIterator
                                                 .flatMap(_.toIterator)
                                                 .partition(_.isDirectory)
        files.map(_.getPath) ++ dirs.flatMap(listFiles)
      }
      val files = for{
        f0 <- Option(listFiles(new java.io.File(path))).toVector
        filename <- f0
        if filename.endsWith(".scala")
        if filter(filename)
        code = io.Source.fromFile(filename).mkString
        if !ScalacParser.checkParseFails(code)
      } yield Future{
        println("Checking: " + filename)
        TestUtil.check(code)
      }


      files.foreach(Await.result(_, Duration.Inf))

    }

    'test - checkDir("scalaparse/shared/src/test/resources")
    def checkRepo(url: String, filter: String => Boolean = _ => true) = {
      import sys.process._
      val name = url.split("/").last
      println("CLONING?")
      if (!Files.exists(Paths.get("target", "repos", name))){
        println("CLONING")
        Seq("git", "clone", url, "target/repos/"+name, "--depth", "1").!
      }
      checkDir("target/repos/"+name, filter)
    }


    'fastparse - checkRepo("https://github.com/lihaoyi/fastparse")
    'scalaJs - checkRepo("https://github.com/scala-js/scala-js")
    'scalaz - checkRepo("https://github.com/scalaz/scalaz")
    'shapeless - checkRepo("https://github.com/milessabin/shapeless")
    'akka - checkRepo("https://github.com/akka/akka")
    'lift - checkRepo("https://github.com/lift/framework")
    'play - checkRepo("https://github.com/playframework/playframework")
    'PredictionIO - checkRepo("https://github.com/PredictionIO/PredictionIO")
    'spark - checkRepo("https://github.com/apache/spark")
    'sbt - checkRepo("https://github.com/sbt/sbt",
      !Seq(
        // Unicode escapes in weird places
        "target/repos/sbt/main/settings/src/main/scala/sbt/std/InputWrapper.scala"
      ).contains(_)
    )
    'cats - checkRepo("https://github.com/non/cats")
    'finagle - checkRepo("https://github.com/twitter/finagle")
    'kafka - checkRepo("https://github.com/apache/kafka")
    'breeze - checkRepo("https://github.com/scalanlp/breeze")
    'spire - checkRepo("https://github.com/non/spire")
    'saddle - checkRepo("https://github.com/saddle/saddle")
    'scala - checkRepo(
      "https://github.com/scala/scala",
      !Seq(
        // This fella seems to make the scalac parser hang (???)
        "target/repos/scala/test/files/neg/t5510.scala",
        // Unicode escapes in weird places
        "target/repos/scala/test/files/neg/t8015-ffb.scala",
        "target/repos/scala/test/files/pos/t389.scala",
        "target/repos/scala/test/files/run/literals.scala",
        "target/repos/scala/test/files/run/t3835.scala",
        // Scalac parser seems to accept this, though it blows up later
        "target/repos/scala/test/files/neg/t8266-invalid-interp.scala"

      ).contains(_)
    )

  }
}