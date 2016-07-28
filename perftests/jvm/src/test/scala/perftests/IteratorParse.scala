package perftests

object IteratorParse {
  val bootstrapSource = scala.io.Source.fromInputStream(
    getClass.getResourceAsStream("/bootstrap.css")
  ).mkString
  val tests = TestSuite {
    val parser = cssparse.CssRulesParser.ruleList
    'Bootstrap {
      val results = Utils.benchmark(Seq(
        () => parser.parse(bootstrapSource),
        () => parser.parse(bootstrapSource + "*/")))

      println("CssParse Benchmark")
      println(results.map(_.mkString(" ")).mkString("\n"))
    }
  }
}
