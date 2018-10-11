package perftests
import utest._
object JsonnetParse extends TestSuite {
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
  val crossValidationSource = names.map(n =>
    scala.io.Source.fromInputStream(getClass.getResourceAsStream("/" + n)).mkString
  ).mkString("[\n", ",\n", "]")
  def crossValidationIterator(size: Int) = crossValidationSource.grouped(size)


  val tests = Tests {
    'CrossValidation - {
      Utils.benchmarkAll(
        "JsonnetParse",
        fastparseParser.document(_),
        crossValidationSource, Some("[ " + crossValidationSource),
        crossValidationIterator
      )
    }
  }
}
