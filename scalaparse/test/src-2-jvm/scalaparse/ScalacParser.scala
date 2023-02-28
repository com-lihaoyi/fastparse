package scalaparse

import scala.tools.nsc.{Global, Settings}

object ScalacParser{
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
  settings.embeddedDefaults[ScalacParser.type]
  settings.classpath.append(files.mkString(":"))

  val global = new Global(settings)

  def checkParseFails(input: String) = this.synchronized{
    val run = new global.Run()
    var fail = false
    import global.syntaxAnalyzer.Offset
    val cu = new global.CompilationUnit(global.newSourceFile(input))
    val parser = new global.syntaxAnalyzer.UnitParser(cu, Nil){
      override def newScanner() = new global.syntaxAnalyzer.UnitScanner(cu, Nil){
        override def error(off: Offset, msg: String) = {
          fail = true
        }
        override def syntaxError(off: Offset, msg: String) = {
          fail = true
        }
        override def incompleteInputError(off: Offset, msg: String) = {
          fail = true
        }
      }
      override def incompleteInputError(msg: String) = {
        fail = true
      }
      override def syntaxError(offset: Offset, msg: String) = {
        fail = true
      }
    }
    parser.parse()
    fail
  }
}