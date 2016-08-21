package cssparse

import java.nio.file.{Files, Paths}

import utest._

import scala.sys.process._

object ProjectTests extends TestSuite {

  def checkCss()(implicit testPath: utest.framework.TestPath) = {
    val url = "https://github.com/" + testPath.value.last
    val name = url.split("/").last

    println(Paths.get("target", "files", name))
    if (!Files.exists(Paths.get("target", "files", name))){
      println("DOWNLOADING")
      Seq("wget", url, "-O", "target/files/" + name).!
    }
    val css = new String(
      java.nio.file.Files.readAllBytes(
        java.nio.file.Paths.get("target", "files", name)
      )
    )
    TestUtil.checkParsing(css, tag = name)
    TestUtil.checkPrinting(css, tag = name)
  }

  val tests = this {
    Seq("mkdir", "-p", "target/files").!

    "twbs/bootstrap/raw/master/dist/css/bootstrap.css" - checkCss()
    "twbs/bootstrap/raw/master/dist/css/bootstrap.min.css" - checkCss()
    "primer/primer/raw/master/css/primer.css" - checkCss()
  }
}
