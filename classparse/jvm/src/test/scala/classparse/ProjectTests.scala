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

  def checkRepo(dirs: Seq[String] = Seq(""), srcFolders: Boolean = true,
                filter: String => Boolean = _ => true)
               (implicit testPath: utest.framework.TestPath) = {
    val url = "https://github.com/" + testPath.value.last
    import sys.process._
    val name = url.split("/").last
    println("CLONING?")
    if (!Files.exists(Paths.get("target", "repos", name))){
      println("CLONING")
      Seq("git", "clone", url, "target/repos/" + name, "--depth", "1").!
    }
    println("CLEANING")
    Seq("mvn", "clean", "-f", "target/repos/" + name + "/pom.xml", "-q").!
    println("COMPILING")
    Seq("mvn", "compile", "-f", "target/repos/" + name + "/pom.xml", "-q").!
    checkDir("target/repos/" + name, dirs, srcFolders, filter)
  }

  val tests = TestSuite {
    "junit-team/junit4" - checkRepo(filter = !_.endsWith("package-info.java"))
    "checkstyle/checkstyle" - checkRepo(filter = !_.endsWith("package-info.java"))
    "jenkinsci/jenkins" - checkRepo(dirs = Seq("/core", "/cli"), filter = !_.endsWith("package-info.java"))
    "immutables/immutables" - checkRepo(
      dirs = Seq("/utility", "/testing", "/generator", "/generator-processor", "/generator-fixture",
                 "/metainf", "/mirror", "/value", "/ordinal", "/builder", "/android-stub", "/gson", "/mongo",
                 "/func", "/value-processor", "/value-fixture", "/serial", "/cases", "/encode"),
      srcFolders = false,
      filter = !_.endsWith("package-info.java"))
    "JodaOrg/joda-time" - checkRepo(filter = !_.endsWith("package-info.java"))
    "libgdx/libgdx" - checkRepo(dirs = Seq("/gdx"), srcFolders = false, filter = !_.endsWith("package-info.java"))
  }

}
