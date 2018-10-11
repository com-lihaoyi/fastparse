package perftests.string

import perftests.Utils
import utest._
import fastparse._, NoWhitespace._
object PythonParse extends TestSuite {
  val crossValidationStream = getClass.getResourceAsStream("/cross_validation.py")
  val crossValidationSource = scala.io.Source.fromInputStream(crossValidationStream).mkString
  def crossValidationIterator(size: Int) = crossValidationSource.grouped(size)
  def parser[_: P] = pythonparse.Statements.file_input ~ End
  val tests = Tests {
    'CrossValidation - {
      Utils.benchmarkAll(
        "PythonParse",
        parser(_),
        crossValidationSource, Some(crossValidationSource + " def"),
        crossValidationIterator
      )
    }
  }
}
