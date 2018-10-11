{
  local container = $.core.v1.container,

  node_exporter_container::
    container.new('node-exporter', $._images.nodeExporter) +
    container.withPorts($.core.v1.containerPort.new('http-metrics', 9100)) +
    container.withArgs([
      '--path.procfs=/host/proc',
      '--path.sysfs=/host/sys',
      '--collector.filesystem.ignored-mount-points=^/(sys|proc|dev|host|etc)($|/)',
    ]) +
    container.mixin.securityContext.withPrivileged(true) +
    container.mixin.securityContext.withRunAsUser(0) +
    $.util.resourcesRequests('10m', '20Mi') +
    $.util.resourcesLimits('50m', '40Mi'),

  local daemonSet = $.extensions.v1beta1.daemonSet,

  node_exporter_daemonset:
    daemonSet.new('node-exporter', [$.node_exporter_container]) +
    daemonSet.mixin.spec.template.spec.withHostPid(true) +
    daemonSet.mixin.spec.template.spec.withHostNetwork(true) +
    daemonSet.mixin.spec.template.metadata.withAnnotationsMixin({ 'prometheus.io.scrape': 'false' }) +
    $.util.hostVolumeMount('proc', '/proc', '/host/proc') +
    $.util.hostVolumeMount('sys', '/sys', '/host/sys') +
    (if $._config.node_exporter_mount_root
     then $.util.hostVolumeMount('root', '/', '/rootfs')
     else {}),
}