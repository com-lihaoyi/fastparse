{
  local policyRule = $.rbac.v1beta1.policyRule,

  prometheus_rbac:
    $.util.rbac('prometheus', [
      policyRule.new() +
      policyRule.withApiGroups(['']) +
      policyRule.withResources(['nodes', 'nodes/proxy', 'services', 'endpoints', 'pods']) +
      policyRule.withVerbs(['get', 'list', 'watch']),

      policyRule.new() +
      policyRule.withNonResourceUrls('/metrics') +
      policyRule.withVerbs(['get']),
    ]),

  local container = $.core.v1.container,

  prometheus_container::
    container.new('prometheus', $._images.prometheus) +
    container.withPorts($.core.v1.containerPort.new('http-metrics', 80)) +
    container.withArgs([
      '--config.file=/etc/prometheus/prometheus.yml',
      '--web.listen-address=:%s' % $._config.prometheus_port,
      '--web.external-url=%s%s' % [$._config.prometheus_external_hostname, $._config.prometheus_path],
      '--web.enable-lifecycle',
      '--web.route-prefix=%s' % $._config.prometheus_web_route_prefix,
    ]) +
    $.util.resourcesRequests('250m', '1536Mi') +
    $.util.resourcesLimits('500m', '2Gi'),

  prometheus_watch_container::
    container.new('watch', $._images.watch) +
    container.withArgs([
      '-v',
      '-t',
      '-p=/etc/prometheus',
      'curl',
      '-X',
      'POST',
      '--fail',
      '-o',
      '-',
      '-sS',
      'http://localhost:%s%s-/reload' % [$._config.prometheus_port, $._config.prometheus_web_route_prefix],
    ]),

  local deployment = $.apps.v1beta1.deployment,

  prometheus_deployment:
    if $._config.stateful
    then {}
    else (
      deployment.new('prometheus', 1, [
        $.prometheus_container,
        $.prometheus_watch_container,
      ]) +
      $.util.configVolumeMount('prometheus-config', '/etc/prometheus') +
      deployment.mixin.spec.template.metadata.withAnnotations({ 'prometheus.io.path': '%smetrics' % $._config.prometheus_web_route_prefix }) +
      deployment.mixin.spec.template.spec.securityContext.withRunAsUser(0) +
      if $._config.enable_rbac
      then deployment.mixin.spec.template.spec.withServiceAccount('prometheus')
      else {}
    ),

  local pvc = $.core.v1.persistentVolumeClaim,

  prometheus_pvc::
    if ! $._config.stateful
    then {}
    else (
      pvc.new() +
      pvc.mixin.metadata.withName('prometheus-data') +
      pvc.mixin.spec.withAccessModes('ReadWriteOnce') +
      pvc.mixin.spec.resources.withRequests({ storage: '300Gi' })
    ),

  local statefulset = $.apps.v1beta1.statefulSet,
  local volumeMount = $.core.v1.volumeMount,

  prometheus_statefulset:
    if ! $._config.stateful
    then {}
    else (
      statefulset.new('prometheus', 1, [
        $.prometheus_container.withVolumeMountsMixin(
          volumeMount.new('prometheus-data', '/prometheus')
        ),
        $.prometheus_watch_container,
      ], $.prometheus_pvc) +
      $.util.configVolumeMount('prometheus-config', '/etc/prometheus') +
      statefulset.mixin.spec.withServiceName('prometheus') +
      statefulset.mixin.spec.template.metadata.withAnnotations({ 'prometheus.io.path': '%smetrics' % $._config.prometheus_web_route_prefix }) +
      statefulset.mixin.spec.template.spec.securityContext.withRunAsUser(0) +
      if $._config.enable_rbac
      then statefulset.mixin.spec.template.spec.withServiceAccount('prometheus')
      else {}
    ),

  prometheus_service:
    $.util.serviceFor(
      if $._config.stateful
      then $.prometheus_statefulset
      else $.prometheus_deployment
    ),
}