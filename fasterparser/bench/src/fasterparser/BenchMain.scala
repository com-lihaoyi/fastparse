package test.scala.fasterparser

import fasterparser.Parse
import test.fasterparser.FasterParserParser
import fasterparser.Parsing._
object BenchMain{

  // Parsing example jsonnet files taken from https://github.com/grafana/jsonnet-libs

  def main(args: Array[String]): Unit = {
    val names = Seq(
      "consul-alerts.jsonnet",
      "consul-dashboards.jsonnet",
      "grafana.jsonnet",
      "ksonnet-util.jsonnet",
      "oauth2-proxy.jsonnet",
      "prometheus.jsonnet",
      "prometheus-alertmanager.jsonnet",
      "prometheus-config.jsonnet",
      "prometheus-config-kops.jsonnet",
      "prometheus-grafana.jsonnet",
      "prometheus-kube-state-metrics.jsonnet",
      "prometheus-nginx.jsonnet",
      "prometheus-node-exporter.jsonnet"
    )
    val bodies =
      for(name <- names)
      yield ammonite.ops.read(ammonite.ops.pwd / 'fasterparser / 'bench / 'resources / 'fasterparser / name)

    val parser = new test.fasterparser.FastParseParser()
    val parser2 = new FasterParserParser()
    for((name, body) <- names.zip(bodies)){
      println(name)
      val oldResult = parser.document.parse(body).get.value
      val newResult = Parse(body).read(parser2.document(_)).get.value
      assert(oldResult == newResult, oldResult + " != " + newResult)

    }


    val start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < 20000){
      count += 1
      bodies.foreach(parser.expr.parse(_))

    }
    println(count)
    val start2 = System.currentTimeMillis()
    var count2 = 0
    while(System.currentTimeMillis() - start2 < 20000){
      count2 += 1
      bodies.foreach(b => Parse(b).read(parser2.document(_))

    }
    println(count2)



  }
}