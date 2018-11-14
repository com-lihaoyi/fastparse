package compare

import org.python.core.{CompileMode, CompilerFlags}
import utest._

object PythonBench extends TestSuite{
  def bench(f: => Unit) = {
    var start = System.currentTimeMillis()
    while(System.currentTimeMillis() - start < 10000) {
      f
    }
    start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < 10000) {
      f
      count += 1
    }
    count
  }
  val txt = scala.io.Source
    .fromFile("perftests/resources/cross_validation.py")
    .mkString

  val tests = Tests{
    'fastparse - bench{
      fastparse.parse(txt, pythonparse.Statements.file_input(_))
    }
    'jython - bench{
      org.python.core.ParserFacade.parse(txt, CompileMode.exec, "<test>", CompilerFlags.getCompilerFlags)
    }
  }
}