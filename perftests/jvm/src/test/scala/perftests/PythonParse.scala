package perftests

import fastparse.all._
import utest._

object PythonParse extends TestSuite {
  val crossValidationSource = scala.io.Source.fromInputStream(
    getClass.getResourceAsStream("/cross_validation.py")
  ).mkString
  val tests = TestSuite {
    val parser = pythonparse.Statements.file_input
    'CrossValidation {
      val results = Utils.benchmark(Seq(
        () => parser.parse(crossValidationSource),
        () => parser.parse("def " + crossValidationSource)))

      println("PythonParse Benchmark")
      println(results.map(_.mkString(" ")).mkString("\n"))
    }
  }
}
