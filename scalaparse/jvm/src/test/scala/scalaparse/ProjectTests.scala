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
    def checkRepo(commitHash: String,
                  filter: String => Boolean = _ => true)
                 (implicit testPath: utest.framework.TestPath) = {

      val repo = "https://github.com/" + testPath.value.last
      import sys.process._
      val name = repo.split("/").last
      println("CLONING?")
      val path = Paths.get("target", "repos", name)
      if (!Files.exists(path)){
        println("CLONING")

        new java.lang.ProcessBuilder()
          .command("git", "clone", repo, path.toString)
          .directory(new java.io.File("."))
          .start()
          .waitFor()

        new java.lang.ProcessBuilder()
          .command("git", "checkout", commitHash)
          .directory(path.toFile)
          .start()
          .waitFor()
      }
      checkDir("target/repos/"+name, filter)
    }


    "lihaoyi/fastparse" - checkRepo("d595bacb4dcc1122fad981cbd722d81165bc9e57")
    "scala-js/scala-js" - checkRepo("b8fc74515491880c7b54ff31b7afb7a8b7fa3311")
    "scalaz/scalaz" - checkRepo("bc19f0ffb8684f5b35aff669f4f37255d8052b4a")
    "milessabin/shapeless" - checkRepo("9a66faeff8bad8a67aca50f13e4175bae5fa56d0")
    "akka/akka"- checkRepo(
      "cd8ec9f9815b066463621cb145d73a9d0b66f6e6",
      // Strange Array(-41, -107) characters
      _ != "target/repos/akka/akka-stream-tests/src/test/scala/akka/stream/scaladsl/FlowGroupedWithinSpec.scala"
    )
    "lift/framework" - checkRepo("50a556c2742b53b7500e65206830a647e12c0e00")
    "playframework/playframework" - checkRepo("f45a73f36333f455e46d872dea9d43138f19e907")
    "PredictionIO/PredictionIO" - checkRepo("9017d7fae4a77246b63688508c3d56dedad177e0")
    "apache/spark" - checkRepo("014dc8471200518d63005eed531777d30d8a6639")
    "sbt/sbt" - checkRepo(
      "4ce4fb72bde3b8acfaf526b79d32ca1463bc687b",
      x => !Seq(
        // Unicode escapes in weird places
        "target/repos/sbt/main-settings/src/main/scala/sbt/std/InputWrapper.scala",
        // uses a package called `macro`
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/inherited-macros",
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/macro"
      ).exists(x.startsWith)
    )
    "non/cats" - checkRepo("040817006ae64392a6bd0e25d32069f47b207e49")
    "twitter/finagle" - checkRepo("ade45cf58451f6e372665ac27476d50c5c5d34c9")
    "apache/kafka" - checkRepo("c2d9a2f307ce0b221c915030a1b3831aa4d3763c")
    "scalanlp/breeze" - checkRepo("6aab3d31e7c2b5b9c40f405051d9fb43654e6826")
    "non/spire" - checkRepo("b5952220567830a7817aa37109109b4dbc9ddb83")
    "saddle/saddle" - checkRepo("99bdb486361b9204df6a2dc45de86ce92803ede5")
    "scala-ide/scala-ide" - checkRepo("71a28d0e138171e6518e7bc5adb6d0bc00a2c743")
    "scalafx/scalafx" - checkRepo("1ccaca58a936660fd14f02bb85bea7891dfbb8f7")
    "scalafx/scalafx-ensemble"- checkRepo("b7513a3f9213d53a1ea99958d24f0978e68b7677")
    "takezoe/gitbucket" - checkRepo("24d4763fc81d6f2d83542051d7cedeed3b4742ae")
    // "twitter/scalding" - checkRepo() // disabled due to https://github.com/lihaoyi/fastparse/issues/162
    "pocorall/scaloid" - checkRepo("b4fcbd0c25870941af727b382cee500600b7594e")
    "mesosphere/marathon" - checkRepo("6d5c5b53ddd63aa2ee2a4e504d1dfd4159dde31c")
    "scalatra/scalatra" - checkRepo("d997d15cc791a1043d60969c27d468ea1a2ea1c3")
    "slick/slick" - checkRepo("dcd5bcfa1f52192339c6eb9b264673b1eb893560")
    "ensime/ensime-server" - checkRepo("3db871f41b30572225cdce5a33ffa779721f915b")
    "GravityLabs/goose" - checkRepo("462f04a0b3d79508266770fd2462b1d4b43f6c54")
    "ornicar/lila" - checkRepo(
      "3f093d56e5000560a5db83e9d26db8e5143f2a80",
      x => !Seq(
        "target/repos/lila/modules/lobby/src/main/SocketHandler.scala"
      ).exists(x.startsWith)
    )

    "precog/platform" - checkRepo("9655622c4969a025ac198674d06b287fa5bbb8d1")
    "twitter/util" - checkRepo("9fa550a269d2287b24e94921a352ba954f9f4bfb")
    "scala/pickling" - checkRepo("f7c64bc11f11e78e80ff326da9fbc4fa8d045a80")
    // takes forever to clone on crappy internet =/
    "JetBrains/intellij-scala" - checkRepo(
      "e01bf0eb1a40e7216923f233b6d79121a4663432",
      x => !Seq(
        // Uses unicode escapes
        "target/repos/intellij-scala/scala/scala-impl/testdata/scalacTests/failed/t389.scala"
      ).exists(x.startsWith)
    )
    "scalatest/scalatest" - checkRepo(
      "6e03d4d77462d0f1ab2fc211c22c03c4150b9a30",
      x => !Seq(
        // Unicode escapes in weird places
        "target/repos/scalatest/common-test/src/main/scala/org/scalatest/OperatorNames.scala",
        "target/repos/scalatest/scalatest-test/src/test/scala/org/scalatest/OperatorNames.scala"
      ).exists(x.startsWith)
    )
    "macroid/macroid" - checkRepo("7ec047317190d8f0a2a22df69f4c015b0181a4f2")
    // annoyingly uses trailing .s all over the place, needing dozens of
    // skipped files. Probably only run this after we can properly parse those
//    'delite- checkRepo("https://github.com/stanford-ppl/Delite",
//      x => !Seq(
//        // trailing . after number
//        "target/repos/Delite/apps/multi-dsl/src/ppl/apps/interop/CustomerPricing.scala",
//        "target/repos/Delite/apps/optiml"
//      ).exists(x.startsWith))
    "ucb-bar/chisel" - checkRepo("d9ae7973fa7c60bc744723c1d754c669b456dc65")
    "etorreborre/specs2" - checkRepo("2c0d6d9cc518a08ac6861c4e4ea9c8def2b37157")
    "scala/scala" - checkRepo(
      "c2a5883891a68180b143eb462c8b0cebc8d3b021",
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
