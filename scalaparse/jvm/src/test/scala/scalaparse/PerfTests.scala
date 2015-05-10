package scalaparse

import utest._

import scala.tools.nsc.{Global, Settings}

object PerfTests extends TestSuite{
  val genJsCodeSource = scala.io.Source.fromInputStream(
    getClass.getResourceAsStream("/scalaparser/GenJSCode.scala")
  ).mkString
  val tests = TestSuite{
    'GenJSCode{
      var current = Thread.currentThread().getContextClassLoader
      val files = collection.mutable.Buffer.empty[java.io.File]
      files.appendAll(
        System.getProperty("sun.boot.class.path")
          .split(":")
          .map(new java.io.File(_))
      )
      while(current != null){
        current match{
          case t: java.net.URLClassLoader =>
            files.appendAll(t.getURLs.map(u => new java.io.File(u.toURI)))
          case _ =>
        }
        current = current.getParent
      }

      val settings = new Settings()
      settings.usejavacp.value = true
      settings.classpath.append(files.mkString(":"))
      val global = new Global(settings)
      val run = new global.Run()

      println("Optimizing Parser")

      val parser = Scala.CompilationUnit
      println("Loaded " + genJsCodeSource.length + " bytes of input. Parsing...")
      // Last Run
      // (384,336,5107,434,338,5569,424,341,5544)
      (
//        time(() => parser.parse(genJsCodeSource, trace = false)),
//        time(() => parser.parse(genJsCodeSource, trace = true)),
//        time(() => global.newUnitParser(genJsCodeSource).parse()),
//        time(() => parser.parse(genJsCodeSource, trace = false)),
//        time(() => parser.parse(genJsCodeSource, trace = true)),
//        time(() => global.newUnitParser(genJsCodeSource).parse()),
//        time(() => parser.parse(genJsCodeSource, trace = false)),
//        time(() => parser.parse(genJsCodeSource, trace = true)),
//        time(() => global.newUnitParser(genJsCodeSource).parse())
      )
    }
  }
  def time(f: () => Unit) = {
    val start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < 30000){
      f()
      count += 1
    }
    count
  }
}

