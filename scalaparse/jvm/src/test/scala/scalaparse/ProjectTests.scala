package scalaparse

import java.io.File
import java.nio.file.{Files, Path, Paths}

import fastparse.utils.IndexedParserInput

import concurrent.ExecutionContext.Implicits.global
import utest._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ProjectTests extends TestSuite{

  println("running")
  def tests = this{

    def checkDir(path: String, filter: String => Boolean = _ => true) = {
      println("Checking Dir " + path)
      def listFiles(s: File): Seq[String] = {
        val (dirs, files) = Option(s.listFiles).getOrElse(Array[File]()).partition(_.isDirectory)

        files.map(_.getPath) ++ dirs.flatMap(listFiles)
      }

      val files = for {
        filename <- listFiles(new File(path))
      } yield Future{
        if (filename.endsWith(".scala") && filter(filename)) {
          val code = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(filename)))
          if (!ScalacParser.checkParseFails(code)) {
            print(".")
            TestUtil.check(code, tag = filename, skipIterator = true)
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
    def checkRepo(filter: String => Boolean = _ => true)(implicit testPath: utest.framework.TestPath) = {
      val url = "https://github.com/" + testPath.value.last
      import sys.process._
      val name = url.split("/").last
      println("CLONING?")
      if (!Files.exists(Paths.get("target", "repos", name))){
        println("CLONING")
        Seq("git", "clone", url, "target/repos/"+name, "--depth", "1").!
      }
      checkDir("target/repos/"+name, filter)
    }


    "lihaoyi/fastparse" - checkRepo()
    "scala-js/scala-js" - checkRepo()
    "scalaz/scalaz" - checkRepo()
    "milessabin/shapeless" - checkRepo()
    "akka/akka"- checkRepo()
    "lift/framework" - checkRepo()
    "playframework/playframework" - checkRepo()
    "PredictionIO/PredictionIO" - checkRepo()
    "apache/spark" - checkRepo()
    "sbt/sbt" - checkRepo(
      x => !Seq(
        // Unicode escapes in weird places
        "target/repos/sbt/main-settings/src/main/scala/sbt/std/InputWrapper.scala",
        // uses a package called `macro`
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/inherited-macros",
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/macro"
      ).exists(x.startsWith)
    )
    "non/cats" - checkRepo()
    "twitter/finagle" - checkRepo()
    "apache/kafka" - checkRepo()
    "scalanlp/breeze" - checkRepo()
    "non/spire" - checkRepo()
    "saddle/saddle" - checkRepo()
    "scala-ide/scala-ide" - checkRepo()
    "scalafx/scalafx" - checkRepo()
    "scalafx/scalafx-ensemble"- checkRepo()
    "takezoe/gitbucket" - checkRepo()
    "twitter/scalding" - checkRepo()
    "pocorall/scaloid" - checkRepo()
    "mesosphere/marathon" - checkRepo()
    "scalatra/scalatra" - checkRepo()
    "slick/slick" - checkRepo()
    "ensime/ensime-server" - checkRepo()
    "GravityLabs/goose" - checkRepo()
    "ornicar/lila" - checkRepo(
      x => !Seq(
        "target/repos/lila/modules/lobby/src/main/SocketHandler.scala"
      ).exists(x.startsWith)
    )
    "precog/platform" - checkRepo()
    "twitter/util" - checkRepo()
    "scala/pickling" - checkRepo()
    // takes forever to clone on crappy internet =/
    "JetBrains/intellij-scala" - checkRepo()
    "scalatest/scalatest" - checkRepo(
      x => !Seq(
        // Unicode escapes in weird places
        "target/repos/scalatest/common-test/src/main/scala/org/scalatest/OperatorNames.scala",
        "target/repos/scalatest/scalatest-test/src/test/scala/org/scalatest/OperatorNames.scala"
      ).exists(x.startsWith)
    )
    "macroid/macroid" - checkRepo()
    // annoyingly uses trailing .s all over the place, needing dozens of
    // skipped files. Probably only run this after we can properly parse those
//    'delite- checkRepo("https://github.com/stanford-ppl/Delite",
//      x => !Seq(
//        // trailing . after number
//        "target/repos/Delite/apps/multi-dsl/src/ppl/apps/interop/CustomerPricing.scala",
//        "target/repos/Delite/apps/optiml"
//      ).exists(x.startsWith))
    "ucb-bar/chisel" - checkRepo()
    "etorreborre/specs2" - checkRepo()
    "scala/scala" - checkRepo(
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
        "target/repos/scala/test/files/presentation/infix-completion/src/Snippet.scala",
        // Not sure why this is failing but it's new, and earlier version of Scalaparse fail too
        "target/repos/scala/src/scaladoc/scala/tools/nsc/doc/html/page/Entity.scala",
        "target/repos/scala/src/scaladoc/scala/tools/nsc/doc/html/HtmlPage.scala",
        "target/repos/scala/src/scaladoc/scala/tools/nsc/doc/html/page/Template.scala"
      ).exists(x.startsWith)
    )

  }
}
