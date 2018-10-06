{
  prometheus_config:: {
    global: {
      scrape_interval: '15s',
    },

    rule_files: [
      'alerts.rules',
      'recording.rules',
    ],

    alerting: {
      alertmanagers: [{
        kubernetes_sd_configs: [{
          role: 'pod',
        }],
        path_prefix: $._config.alertmanager_path,
        tls_config: {
          ca_file: '/var/run/secrets/kubernetes.io/serviceaccount/ca.crt',
        },
        bearer_token_file: '/var/run/secrets/kubernetes.io/serviceaccount/token',
        relabel_configs: [{
          source_labels: ['__meta_kubernetes_pod_label_name'],
          regex: 'alertmanager',
          action: 'keep',
        }, {
          source_labels: ['__meta_kubernetes_namespace'],
          regex: $._config.namespace,
          action: 'keep',
        }, {
          source_labels: ['__meta_kubernetes_pod_container_port_number'],
          regex: null,
          action: 'drop',
        }],
      }],
    },

    scrape_configs: [
      {
        job_name: 'kubernetes-pods',
        kubernetes_sd_configs: [{
          role: 'pod',
        }],

        tls_config: {
          ca_file: '/var/run/secrets/kubernetes.io/serviceaccount/ca.crt',
          insecure_skip_verify: $._config.prometheus_insecure_skip_verify,
        },
        bearer_token_file: '/var/run/secrets/kubernetes.io/serviceaccount/token',

        // You can specify the following annotations (on pods):
        //   prometheus.io.scrape: false - don't scrape this pod
        //   prometheus.io.scheme: https - use https for scraping
        //   prometheus.io.port - scrape this port
        //   prometheus.io.path - scrape this path
        //   prometheus.io.param-<parameter> - send ?parameter=value with the scrape
        relabel_configs: [

          // Drop anything annotated with prometheus.io.scrape=false
          {
            source_labels: ['__meta_kubernetes_pod_annotation_prometheus_io_scrape'],
            action: 'drop',
            regex: 'false',
          },

          // Drop any endpoint who's pod port name does not end with metrics
          {
            source_labels: ['__meta_kubernetes_pod_container_port_name'],
            action: 'keep',
            regex: '.*-metrics',
          },

          // Allow pods to override the scrape scheme with prometheus.io.scheme=https
          {
            source_labels: ['__meta_kubernetes_pod_annotation_prometheus_io_scheme'],
            action: 'replace',
            target_label: '__scheme__',
            regex: '^(https?)$',
            replacement: '$1',
          },

          // Allow service to override the scrape path with prometheus.io.path=/other_metrics_path
          {
            source_labels: ['__meta_kubernetes_pod_annotation_prometheus_io_path'],
            action: 'replace',
            target_label: '__metrics_path__',
            regex: '^(.+)$',
            replacement: '$1',
          },

          // Allow services to override the scrape port with prometheus.io.port=1234
          {
            source_labels: ['__address__', '__meta_kubernetes_pod_annotation_prometheus_io_port'],
            action: 'replace',
            target_label: '__address__',
            regex: '(.+?)(\\:\\d+)?;(\\d+)',
            replacement: '$1:$3',
          },

          // Drop pods without a name label
          {
            source_labels: ['__meta_kubernetes_pod_label_name'],
            action: 'drop',
            regex: '^$',
          },

          // Rename jobs to be <namespace>/<name, from pod name label>
          {
            source_labels: ['__meta_kubernetes_namespace', '__meta_kubernetes_pod_label_name'],
            action: 'replace',
            separator: '/',
            target_label: 'job',
            replacement: '$1',
          },

          // But also include the namespace as a separate label, for routing alerts
          {
            source_labels: ['__meta_kubernetes_namespace'],
            action: 'replace',
            target_label: 'namespace',
          },

          // Rename instances to be the pod name
          {
            source_labels: ['__meta_kubernetes_pod_name'],
            action: 'replace',
            target_label: 'instance',
          },

          {
            regex: '__meta_kubernetes_pod_annotation_prometheus_io_param_(.+)',
            action: 'labelmap',
            replacement: '__param_$1',
          },
        ],
      },

      // A separate scrape config for kube-state-metrics which doesn't add a namespace
      // label, instead taking the namespace label from the exported timeseries.  This
      // prevents the exported namespace label being renamed to exported_namespace, and
      // allows us to route alerts based on namespace.
      {
        job_name: '%s/kube-state-metrics' % $._config.namespace,
        kubernetes_sd_configs: [{
          role: 'pod',
          namespaces: {
            names: [$._config.namespace],
          },
        }],

        tls_config: {
          ca_file: '/var/run/secrets/kubernetes.io/serviceaccount/ca.crt',
          insecure_skip_verify: $._config.prometheus_insecure_skip_verify,
        },
        bearer_token_file: '/var/run/secrets/kubernetes.io/serviceaccount/token',

        relabel_configs: [

          // Drop anything who's service is not kube-state-metrics
          // Rename jobs to be <namespace>/<name, from pod name label>
          {
            source_labels: ['__meta_kubernetes_pod_label_name'],
            regex: 'kube-state-metrics',
            action: 'keep',
          },

          // Rename instances to be the pod name
          {
            source_labels: ['__meta_kubernetes_pod_name'],
            action: 'replace',
            target_label: 'instance',
          },
        ],
      },

      // A separate scrape config for node-exporter which maps the nodename onto the
      // instance label.
      {
        job_name: '%s/node-exporter' % $._config.namespace,
        kubernetes_sd_configs: [{
          role: 'pod',
          namespaces: {
            names: [$._config.namespace],
          },
        }],

        tls_config: {
          ca_file: '/var/run/secrets/kubernetes.io/serviceaccount/ca.crt',
          insecure_skip_verify: $._config.prometheus_insecure_skip_verify,
        },
        bearer_token_file: '/var/run/secrets/kubernetes.io/serviceaccount/token',

        relabel_configs: [
          // Drop anything who's name is not node-exporter.
          {
            source_labels: ['__meta_kubernetes_pod_label_name'],
            regex: 'node-exporter',
            action: 'keep',
          },

          // Rename instances to be the node name.
          {
            source_labels: ['__meta_kubernetes_pod_node_name'],
            action: 'replace',
            target_label: 'instance',
          },

          // But also include the namespace as a separate label, for routing alerts
          {
            source_labels: ['__meta_kubernetes_namespace'],
            action: 'replace',
            target_label: 'namespace',
          },
        ],
      },

      // This scrape config gather all kubelet metrics.
      {
        job_name: 'kube-system/kubelet',
        kubernetes_sd_configs: [{
          role: 'node',
        }],

        // Couldn't get prometheus to validate the kublet cert for scraping, so don't bother for now
        tls_config: {
          ca_file: '/var/run/secrets/kubernetes.io/serviceaccount/ca.crt',
          insecure_skip_verify: $._config.prometheus_insecure_skip_verify,
        },
        bearer_token_file: '/var/run/secrets/kubernetes.io/serviceaccount/token',

        relabel_configs: [
          {
            target_label: '__address__',
            replacement: $._config.prometheus_api_server_address,
          },
          {
            target_label: '__scheme__',
            replacement: 'https',
          },
          {
            source_labels: ['__meta_kubernetes_node_name'],
            regex: '(.+)',
            target_label: '__metrics_path__',
            replacement: '/api/v1/nodes/${1}/proxy/metrics',
          },
        ],
      },

      // As of k8s 1.7, cAdvisor is on a port 4194.
      {
        job_name: 'kube-system/cadvisor',
        kubernetes_sd_configs: [{
          role: 'node',
        }],

        bearer_token_file: '/var/run/secrets/kubernetes.io/serviceaccount/token',
        tls_config: {
          ca_file: '/var/run/secrets/kubernetes.io/serviceaccount/ca.crt',
          insecure_skip_verify: $._config.prometheus_insecure_skip_verify,
        },

        relabel_configs: [{
          source_labels: ['__address__'],
          regex: '(.+):([0-9]+)',
          target_label: '__address__',
          replacement: '$1:4194',
        }],

        // Drop container_* metrics with no image.
        metric_relabel_configs: [{
          source_labels: ['__name__', 'image'],
          regex: 'container_([a-z_]+);',
          action: 'drop',
        }],
      },

      // If running on GKE, you cannot scrape API server pods, and must instead
      // scrape the API server service.
      {
        job_name: 'default/kubernetes',
        kubernetes_sd_configs: [{
          role: 'endpoints',
        }],
        scheme: 'https',

        bearer_token_file: '/var/run/secrets/kubernetes.io/serviceaccount/token',
        tls_config: {
          ca_file: '/var/run/secrets/kubernetes.io/serviceaccount/ca.crt',
          insecure_skip_verify: $._config.prometheus_insecure_skip_verify,
        },

        relabel_configs: [{
          source_labels: ['__meta_kubernetes_service_label_component'],
          regex: 'apiserver',
          action: 'keep',
        }],
      },
    ],
  },

  // Extension points for adding alerts, recording rules and prometheus config.
  prometheus_alerts:: {
    groups+: [
      {
        name: 'prometheus-extra',
        rules: [
          {
            alert: 'PromScrapeFailed',
            expr: |||
              up != 1
            |||,
            'for': '15m',
            labels: {
              severity: 'warning',
            },
            annotations: {
              message: 'Prometheus failed to scrape a target {{ $labels.job }} / {{ $labels.instance }}',
            },
          },
        ],
      },
    ],
  },

  prometheus_rules:: {
    groups+: [
      {
        // Add mapping from namespace, pod -> node with node name as pod, as
        // we use the node name as the node-exporter instance label.
        name: 'instance_override',
        rules: [
          {
            record: 'node_namespace_pod:kube_pod_info:',
            expr: |||
              max by(node, namespace, instance) (label_replace(kube_pod_info{job="default/kube-state-metrics"}, "instance", "$1", "node", "(.*)"))
            |||,
          },
        ],
      },
    ],
  },

  // We changes to using camelCase, but here we try and make it backwards compatible.
  prometheusAlerts+:: $.prometheus_alerts,
  prometheusRules+:: $.prometheus_rules,

  local configMap = $.core.v1.configMap,

  prometheus_config_map:
    configMap.new('prometheus-config') +
    configMap.withData({
      'prometheus.yml': $.util.manifestYaml($.prometheus_config),
      'alerts.rules': $.util.manifestYaml($.prometheusAlerts),
      'recording.rules': $.util.manifestYaml($.prometheusRules),
    }),
}