{
  grafana_config:: {
    sections: {
      'auth.anonymous': {
        enabled: true,
        org_role: 'Admin',
      },
      server: {
        http_port: 80,
        root_url: $._config.grafana_root_url,
      },
      analytics: {
        reporting_enabled: false,
      },
      users: {
        default_theme: 'light',
      },
    },
  },

  local configMap = $.core.v1.configMap,

  grafana_config_map:
    configMap.new('grafana-config') +
    configMap.withData({ 'grafana.ini': std.manifestIni($.grafana_config) }),

  // Extension point for you to add your own dashboards.
  dashboards+:: {},
  grafana_dashboards+:: {},
  grafanaDashboards+:: $.dashboards + $.grafana_dashboards,

  dashboards_config_map:
    configMap.new('dashboards') +
    configMap.withDataMixin({
      [name]: std.toString($.grafanaDashboards[name])
      for name in std.objectFields($.grafanaDashboards)
    }),

  grafana_dashboard_provisioning_config_map:
    configMap.new('grafana-dashboard-provisioning') +
    configMap.withData({
      'dashboards.yml': $.util.manifestYaml({
        apiVersion: 1,
        providers: [{
          name: 'dashboards',
          orgId: 1,
          folder: '',
          type: 'file',
          disableDeletion: true,
          editable: false,
          options: {
            path: '/grafana/dashboards',
          },
        }],
      }),
    }),

  grafana_datasource_config_map:
    configMap.new('grafana-datasources') +
    $.grafana_add_datasource('prometheus',
                             'http://prometheus.%(namespace)s.svc.%(cluster_dns_suffix)s%(prometheus_web_route_prefix)s' % $._config,
                             default=true),

  grafana_add_datasource(name, url, default=false)::
    configMap.withDataMixin({
      ['%s.yml' % name]: $.util.manifestYaml({
        apiVersion: 1,
        datasources: [{
          name: name,
          type: 'prometheus',
          access: 'proxy',
          url: url,
          isDefault: default,
          version: 1,
          editable: false,
        }],
      }),
    }),

  grafana_add_datasource_with_basicauth(name, url, username, password, default=false)::
    configMap.withDataMixin({
      ['%s.yml' % name]: $.util.manifestYaml({
        apiVersion: 1,
        datasources: [{
          name: name,
          type: 'prometheus',
          access: 'proxy',
          url: url,
          isDefault: default,
          version: 1,
          editable: false,
          basicAuth: true,
          basicAuthUser: username,
          basicAuthPassword: password,
        }],
      }),
    }),

  local container = $.core.v1.container,

  grafana_container::
    container.new('grafana', $._images.grafana) +
    container.withPorts($.core.v1.containerPort.new('grafana', 80)) +
    container.withCommand([
      '/usr/share/grafana/bin/grafana-server',
      '--homepath=/usr/share/grafana',
      '--config=/etc/grafana-config/grafana.ini',
    ]) +
    $.util.resourcesRequests('10m', '40Mi'),

  local deployment = $.apps.v1beta1.deployment,

  grafana_deployment:
    deployment.new('grafana', 1, [$.grafana_container]) +
    deployment.mixin.spec.template.spec.securityContext.withRunAsUser(0) +
    $.util.configVolumeMount('grafana-config', '/etc/grafana-config') +
    $.util.configVolumeMount('grafana-dashboard-provisioning', '%(grafana_provisioning_dir)s/dashboards' % $._config) +
    $.util.configVolumeMount('grafana-datasources', '%(grafana_provisioning_dir)s/datasources' % $._config) +
    $.util.configVolumeMount('dashboards', '/grafana/dashboards'),

  grafana_service:
    $.util.serviceFor($.grafana_deployment),
}