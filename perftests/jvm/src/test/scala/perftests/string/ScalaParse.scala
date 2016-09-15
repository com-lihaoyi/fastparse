package perftests.string

import perftests.Utils
import utest._

import scala.tools.nsc.{Global, Settings}
import scalaparse.{Scala, ScalacParser}
import fastparse.all._
object ScalaParse extends TestSuite{
  val genJsCodeStream = getClass.getResourceAsStream("/GenJSCode.scala")
  val genJsCodeSource = scala.io.Source.fromInputStream(genJsCodeStream).mkString
  def genJsCodeIterator(size: Int) = genJsCodeSource.grouped(size)

  val tests = TestSuite{
    'GenJSCode {
      var current = Thread.currentThread().getContextClassLoader
      val files = collection.mutable.Buffer.empty[java.io.File]
      files.appendAll(
        System.getProperty("sun.boot.class.path")
          .split(":")
          .map(new java.io.File(_))
      )
      while (current != null) {
        current match {
          case t: java.net.URLClassLoader =>
            files.appendAll(t.getURLs.map(u => new java.io.File(u.toURI)))
          case _ =>
        }
        current = current.getParent
      }

      val settings = new Settings()
      settings.usejavacp.value = true
      settings.embeddedDefaults[ScalacParser.type]
      settings.classpath.append(files.mkString(":"))
      val global = new Global(settings)
      val run = new global.Run()

      println("Optimizing Parser")

      val parser = Scala.CompilationUnit
      println("Loaded " + genJsCodeSource.length + " bytes of input. Parsing...")

      /**
        * Parboiled2 ( run separately in fork of parboiled project )
        *
        * avg 1354.25 dev 7.97392
        * 1212
        * 1361
        * 1357
        * 1363
        * 1353
        * 1344
        *
        * FastParse
        *
        * avg 320.75 dev 15.43535
        * 321
        * 343
        * 341
        * 322
        * 316
        * 304
        * FastParseNoTrace
        *
        * avg 434.75 dev 23.4005
        * 334
        * 455
        * 456
        * 414
        * 454
        * 415
        * Scalac
        *
        * avg 4888.25 dev 113.26481
        * 5004
        * 5340
        * 4937
        * 5022
        * 4769
        * 4825
        */

      Utils.benchmarkAll("ScalaParse",
        parser,
        genJsCodeSource, Some(genJsCodeSource + "*/"),
        genJsCodeIterator)
    }
  }
}

