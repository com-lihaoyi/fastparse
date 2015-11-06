package scalaparse

import java.nio.file.{Paths, Path, Files}
import concurrent.ExecutionContext.Implicits.global
import utest._

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scalaparse.PerfTests._

object ProjectTests extends TestSuite{

  println("running")
  def tests = TestSuite{

    def checkDir(path: String, filter: String => Boolean = _ => true) = {
      println("Checking Dir " + path)
      def listFiles(s: java.io.File): Iterator[String] = {
        val (dirs, files) = Option(s.listFiles()).toIterator
                                                 .flatMap(_.toIterator)
                                                 .partition(_.isDirectory)

        files.map(_.getPath) ++ dirs.flatMap(listFiles)
      }

      val files = for{
        f0 <- Option(listFiles(new java.io.File(path))).toVector
        filename <- f0
      } yield Future{
        if (filename.endsWith(".scala") && filter(filename)) {
          val code = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(filename)))
          if (!ScalacParser.checkParseFails(code)) {
            print(".")
            TestUtil.check(code, tag = filename)
          }
        }
      }


      files.foreach(Await.result(_, Duration.Inf))
      println()
    }

    'test - {
      val testSource = scala.io.Source.fromInputStream(
        getClass.getResourceAsStream("/scalaparse/Test.scala")
      ).mkString
      TestUtil.check(testSource)
    }
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
      x => !Seq(
        // Unicode escapes in weird places
        "target/repos/sbt/main/settings/src/main/scala/sbt/std/InputWrapper.scala",
        // uses a package called `macro`
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/inherited-macros",
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/macro"
      ).exists(x.startsWith)
    )
    'cats - checkRepo("https://github.com/non/cats")
    'finagle - checkRepo("https://github.com/twitter/finagle")
    'kafka - checkRepo("https://github.com/apache/kafka")
    'breeze - checkRepo("https://github.com/scalanlp/breeze")
    'spire - checkRepo("https://github.com/non/spire")
    'saddle - checkRepo("https://github.com/saddle/saddle")
    'scalaIDE - checkRepo("https://github.com/scala-ide/scala-ide")
    'gitbucket - checkRepo("https://github.com/takezoe/gitbucket")
    'scalding - checkRepo("https://github.com/twitter/scalding")
    'scaloid - checkRepo("https://github.com/pocorall/scaloid")
    'marathon - checkRepo("https://github.com/mesosphere/marathon")
    'scalatra - checkRepo("https://github.com/scalatra/scalatra")
    'summingbird - checkRepo("https://github.com/twitter/summingbird")
    'slick - checkRepo("https://github.com/slick/slick")
    'ensime - checkRepo("https://github.com/ensime/ensime-server")
    'goose - checkRepo("https://github.com/GravityLabs/goose")
    'lila - checkRepo("https://github.com/ornicar/lila",
      x => !Seq(
       "target/repos/lila/modules/lobby/src/main/SocketHandler.scala"
      ).exists(x.startsWith)
    )
    'precog - checkRepo("https://github.com/precog/platform")
    'twitterUtil - checkRepo("https://github.com/twitter/util")
    'pickling - checkRepo("https://github.com/scala/pickling")
    // takes forever to clone on crappy internet =/
    'intellij - checkRepo("https://github.com/JetBrains/intellij-scala")
    'scalatest - checkRepo("https://github.com/scalatest/scalatest",
      x => !Seq(
        // Unicode escapes in weird places
        "target/repos/scalatest/common-test/src/main/scala/org/scalatest/OperatorNames.scala",
        "target/repos/scalatest/scalatest-test/src/test/scala/org/scalatest/OperatorNames.scala"
      ).exists(x.startsWith)
    )
    'macroid - checkRepo("https://github.com/macroid/macroid")
    // annoyingly uses trailing .s all over the place, needing dozens of
    // skipped files. Probably only run this after we can properly parse those
//    'delite- checkRepo("https://github.com/stanford-ppl/Delite",
//      x => !Seq(
//        // trailing . after number
//        "target/repos/Delite/apps/multi-dsl/src/ppl/apps/interop/CustomerPricing.scala",
//        "target/repos/Delite/apps/optiml"
//      ).exists(x.startsWith))
    'chisel - checkRepo("https://github.com/ucb-bar/chisel")
    'specs2 - checkRepo("https://github.com/etorreborre/specs2")
    'scala - checkRepo(
      "https://github.com/scala/scala",
      x => !Seq(
        // This fella seems to make the scalac parser hang (???)
        "target/repos/scala/test/files/neg/t5510.scala",
        // Unicode escapes in weird places
        "target/repos/scala/test/files/neg/t8015-ffb.scala",
        "target/repos/scala/test/files/pos/t389.scala",
        "target/repos/scala/test/files/run/literals.scala",
        "target/repos/scala/test/files/run/t3835.scala",
        // Scalac parser seems to accept this, though it blows up later
        "target/repos/scala/test/files/neg/t8266-invalid-interp.scala",
        "target/repos/scala/test/disabled/",
        "target/repos/scala/test/files/neg/",
        // trailing . after number
        "target/repos/scala/test/files/presentation/infix-completion/src/Snippet.scala"


      ).exists(x.startsWith)
    )

  }
}