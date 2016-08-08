package perftests

import utest._

object CssParse extends TestSuite {
  val bootstrapStream = getClass.getResourceAsStream("/bootstrap.css")
  val bootstrapSource = wrapString(scala.io.Source.fromInputStream(bootstrapStream).mkString)
  def bootstrapIterator(size: Int) = bootstrapSource.grouped(size)
  val parser = cssparse.CssRulesParser.ruleList

  val tests = TestSuite {
    'Bootstrap {
      Utils.benchmarkAll[Char]("CssParse",
        parser,
        Seq(bootstrapSource, bootstrapSource ++ "*/"),
        bootstrapIterator)
    }
  }
}
