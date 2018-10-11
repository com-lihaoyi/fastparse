package perftests.string
import fastparse._, NoWhitespace._
import perftests.Utils
import utest._
object CssParse extends TestSuite {
  val bootstrapStream = getClass.getResourceAsStream("/bootstrap.css")
  val bootstrapSource = scala.io.Source.fromInputStream(bootstrapStream).mkString
  def bootstrapIterator(size: Int) = bootstrapSource.grouped(size)

  def parser[_: P] = cssparse.CssRulesParser.ruleList ~ End
  val tests = Tests {
    'Bootstrap - {
      Utils.benchmarkAll(
        "CssParse",
        parser(_),
        bootstrapSource, Some(bootstrapSource + "}"),
        bootstrapIterator
      )
    }
  }
}
