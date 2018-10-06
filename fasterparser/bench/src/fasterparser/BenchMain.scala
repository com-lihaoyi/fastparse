package test.scala.fasterparser

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
      "prometheus-node-exporter.jsonnet",
    )
    val bodies =
      for(name <- names)
      yield ammonite.ops.read(ammonite.ops.pwd / 'fasterparser / 'bench / 'resources / 'fasterparser / name)

    val parser = new test.fasterparser.FastParseParser()
    val start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < 20000){
      count += 1
      bodies.foreach(parser.expr.parse(_))

    }
    println(count)
  }
}