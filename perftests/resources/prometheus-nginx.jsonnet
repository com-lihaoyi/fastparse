
{
  local configMap = $.core.v1.configMap,

  nginx_config_map:
    local vars = {
      location_stanzas: [
        |||
          location ~ ^/%(path)s(/?)(.*)$ {
            proxy_pass      %(url)s$2$is_args$args;
          }
        ||| % service
        for service in $._config.admin_services
      ],
      locations: std.join('\n', self.location_stanzas),
      link_stanzas: [
        |||
          <li><a href="/%(path)s">%(title)s</a></li>
        ||| % service
        for service in $._config.admin_services
      ],
      links: std.join('\n', self.link_stanzas),
    };

    configMap.new('nginx-config') +
    configMap.withData({
      'nginx.conf': |||
        worker_processes     5;  ## Default: 1
        error_log            /dev/stderr;
        pid                  /tmp/nginx.pid;
        worker_rlimit_nofile 8192;

        events {
          worker_connections  4096;  ## Default: 1024
        }

        http {
          default_type application/octet-stream;
          log_format   main '$remote_addr - $remote_user [$time_local]  $status '
            '"$request" $body_bytes_sent "$http_referer" '
            '"$http_user_agent" "$http_x_forwarded_for"';
          access_log   /dev/stderr  main;
          sendfile     on;
          tcp_nopush   on;
          resolver     kube-dns.kube-system.svc.%(cluster_dns_suffix)s;
          server {
            listen 80;
            %(locations)s
            location ~ /(index.html)? {
              root /etc/nginx;
            }
          }
        }
      ||| % ($._config + vars),
      'index.html': |||
        <html>
          <head><title>Admin</title></head>
          <body>
            <h1>Admin</h1>
            <ul>
              %(links)s
            </ul>
          </body>
        </html>
      ||| % vars,
    }),

  local container = $.core.v1.container,

  nginx_container::
    container.new('nginx', $._images.nginx) +
    container.withPorts($.core.v1.containerPort.new('http', 80)) +
    $.util.resourcesRequests('50m', '100Mi'),

  local deployment = $.apps.v1beta1.deployment,

  nginx_deployment:
    deployment.new('nginx', 1, [$.nginx_container]) +
    $.util.configVolumeMount('nginx-config', '/etc/nginx'),

  nginx_service:
    $.util.serviceFor($.nginx_deployment),

  local oauth2_proxy = import 'oauth2_proxy/oauth2-proxy.libsonnet',

  oauth2_proxy:
    if ! $._config.oauth_enabled
    then {}
    else oauth2_proxy {
      _config+:: $._config {
        oauth_upstream: 'http://nginx.%(namespace)s.svc.%(cluster_dns_suffix)s/' % $._config,
      },
    },
}