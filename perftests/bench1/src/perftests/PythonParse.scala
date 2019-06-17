package perftests.string

import perftests.Utils
import utest._
import fastparse.all._
object PythonParse extends TestSuite {
  val crossValidationStream = getClass.getResourceAsStream("/cross_validation.py")
  val crossValidationSource = scala.io.Source.fromInputStream(crossValidationStream).mkString
  def crossValidationIterator(size: Int) = crossValidationSource.grouped(size)
  val parser = pythonparse.Statements.file_input ~ End

  val tests = Tests {
    test("CrossValidation"){
      Utils.benchmarkAll(
        "PythonParse",
        parser,
        crossValidationSource, Some(crossValidationSource + " def"),
        crossValidationIterator
      )
    }
  }
}
