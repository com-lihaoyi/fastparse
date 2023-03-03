package perftests.string
import fastparse._, NoWhitespace._
import perftests.Utils
import utest._
object CssParse extends TestSuite {
  val bootstrapStream = getClass.getResourceAsStream("/bootstrap.css")
  val bootstrapSource = scala.io.Source.fromInputStream(bootstrapStream).mkString
  def bootstrapIterator(size: Int) = bootstrapSource.grouped(size)

  def parser[$: P] = cssparse.CssRulesParser.ruleList ~ End
  val tests = Tests {
    test("Bootstrap"){
      Utils.benchmarkAll(
        "CssParse",
        parser(_),
        bootstrapSource, Some(bootstrapSource + "}"),
        bootstrapIterator
      )
    }
  }
}
