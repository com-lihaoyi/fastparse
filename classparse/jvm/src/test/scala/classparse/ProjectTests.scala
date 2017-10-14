package classparse

import java.io.File
import java.net.URLClassLoader
import java.nio.file.{Files, Paths}

import utest._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global

object ProjectTests extends TestSuite {
  def checkDir(initPath: String, dirs: Seq[String] = Seq("/"), srcFolders: Boolean = true,
               filter: String => Boolean = _ => true) = {
    println("Checking Dir " + initPath)
    def listFiles(s: File): Seq[String] = {
      val (dirs, files) = Option(s.listFiles).getOrElse(Array[File]()).partition(_.isDirectory)

      files.map(_.getPath) ++ dirs.flatMap(listFiles)
    }

    for (dir <- dirs) {
      val path = initPath + dir
      val classesPath = path + "/target/classes/"
      val srcPath = path + (if (srcFolders) "/src/main/java/" else "/src/")
      val classLoader = new URLClassLoader(Array(new File(classesPath).toURI.toURL))

      val files = for {
        filename <- listFiles(new File(srcPath))
      } yield Future {
        if (filename.endsWith(".java") && filter(filename)) {
          val relativePath = filename.stripPrefix(srcPath).stripSuffix(".java")
          val javaClassOpt = TestUtils.TryClass( classLoader.loadClass(relativePath.replace('/', '.')) )
          javaClassOpt match {
            case Some(javaClass) =>
              print(".")
              val classFile = fastparse.byte.all.Bytes.view(
                Files.readAllBytes(Paths.get(classesPath + relativePath + ".class"))
              )
              TestUtils.checkClassAndClassFile(javaClass, classFile)
            case None =>
              print("-")
          }
        }
      }

      files.foreach(Await.result(_, Duration.Inf))
    }
    println()
  }

  def checkRepo(commitHash: String, dirs: Seq[String] = Seq(""), srcFolders: Boolean = true,
                filter: String => Boolean = _ => true)
               (implicit testPath: utest.framework.TestPath) = {
    val url = "https://github.com/" + testPath.value.last
    import sys.process._
    val name = url.split("/").last
    val path = Paths.get("target", "repos", name)
    println("CLONING?")
    if (!Files.exists(path)){
      println("CLONING")
      Seq("git", "clone", url, path.toString, "--depth", "1").!
      new java.lang.ProcessBuilder()
        .command("git", "clone", url, path.toString)
        .directory(new java.io.File("."))
        .start()
        .waitFor()

      new java.lang.ProcessBuilder()
        .command("git", "checkout", commitHash)
        .directory(path.toFile)
        .start()
        .waitFor()
    }
    println("CLEANING")
    Seq("mvn", "clean", "-f", "target/repos/" + name + "/pom.xml", "-q").!
    println("COMPILING")
    Seq("mvn", "compile", "-f", "target/repos/" + name + "/pom.xml", "-q").!
    checkDir("target/repos/" + name, dirs, srcFolders, filter)
  }

  val tests = Tests {
    "junit-team/junit4" - checkRepo(
      "376c2fc3f269eaba580c75cd1689ca2ba16ad202",
      filter = !_.endsWith("package-info.java")
    )
    "checkstyle/checkstyle" - checkRepo(
      "2a5690c28a5ce3af7341428f6b71b6897733cf74",
      filter = !_.endsWith("package-info.java")
    )
    "jenkinsci/jenkins" - checkRepo(
      "ce2d0f97bdbb5fb171443559d210dffef7961dc2",
      dirs = Seq("/core", "/cli"), filter = !_.endsWith("package-info.java")
    )
    "immutables/immutables" - checkRepo(
      "b61c6517b8cbed79770c96c6e508c6ef055c6734",
      dirs = Seq("/utility", "/testing", "/generator", "/generator-processor", "/generator-fixture",
                 "/metainf", "/mirror", "/value", "/ordinal", "/builder", "/android-stub", "/gson", "/mongo",
                 "/func", "/value-processor", "/value-fixture", "/serial", "/cases", "/encode"),
      srcFolders = false,
      filter = !_.endsWith("package-info.java")
    )
    "JodaOrg/joda-time" - checkRepo(
      "e177ea4f2e054f9027a559ec5c0a90d87151aae7",
      filter = !_.endsWith("package-info.java")
    )
    "libgdx/libgdx" - checkRepo(
      "33086db72093482e1982888184e2ee6a3a848bbb",
      dirs = Seq("/gdx"), srcFolders = false, filter = !_.endsWith("package-info.java")
    )
  }

}
