package perftests

import utest._

object PythonParse extends TestSuite {
  val crossValidationStream = getClass.getResourceAsStream("/cross_validation.py")
  val crossValidationSource = scala.io.Source.fromInputStream(crossValidationStream).mkString
  def crossValidationIterator(size: Int) = crossValidationSource.grouped(size)
  val parser = pythonparse.Statements.file_input

  val tests = TestSuite {
    'CrossValidation {
      Utils.benchmarkAll("PythonParse",
        parser,
        Seq(crossValidationSource, "def " + crossValidationSource),
        crossValidationIterator)
    }
  }
}
