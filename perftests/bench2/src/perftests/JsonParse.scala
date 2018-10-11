package perftests
import utest._
object JsonParse extends TestSuite {
  val crossValidationStream = getClass.getResourceAsStream("/fasterparser/test.json")
  val crossValidationSource = scala.io.Source.fromInputStream(crossValidationStream).mkString
  def crossValidationIterator(size: Int) = crossValidationSource.grouped(size)
  import fasterparser._, NoWhitespace._
  def jsonDoc[_: P] = P( test.fasterparser.Json.jsonExpr ~ End )
  val tests = Tests {
    'CrossValidation - {
      Utils.benchmarkAll(
        "JsonParse",
        jsonDoc(_),
        crossValidationSource, Some(crossValidationSource + "]"),
        crossValidationIterator
      )
    }
  }
}
