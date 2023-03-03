package perftests
import utest._
object JsonParse extends TestSuite {
  val crossValidationStream = getClass.getResourceAsStream("/fastparse/test.json")
  val crossValidationSource = scala.io.Source.fromInputStream(crossValidationStream).mkString
  def crossValidationIterator(size: Int) = crossValidationSource.grouped(size)
  import fastparse._, NoWhitespace._
  def jsonDoc[$: P] = P( _root_.test.fastparse.Json.jsonExpr ~ End )
  val tests = Tests {
    test("CrossValidation"){
      Utils.benchmarkAll(
        "JsonParse",
        jsonDoc(_),
        crossValidationSource, Some(crossValidationSource + "]"),
        crossValidationIterator
      )
    }
  }
}
