
local k = import 'ksonnet-util/kausal.libsonnet';

k {
  _config+:: {
    oauth_cookie_secret: error 'Must define a cookie secret',
    oauth_client_id: error 'Must define a client id',
    oauth_client_secret: error 'Must define a client secret',
    oauth_redirect_url: error 'Must define a redirect url',
    oauth_upstream: error 'Must define an upstream',
    oauth_email_domain: '*',
    oauth_pass_basic_auth: 'false',
  },

  _images+:: {
    oauth2_proxy: 'a5huynh/oauth2_proxy:latest',
  },

  local secret = $.core.v1.secret,

  oauth2_proxy_secret:
    secret.new('oauth2-proxy', {
      OAUTH2_PROXY_COOKIE_SECRET: std.base64($._config.oauth_cookie_secret),
      OAUTH2_PROXY_CLIENT_SECRET: std.base64($._config.oauth_client_secret),
      OAUTH2_PROXY_CLIENT_ID: std.base64($._config.oauth_client_id),
    }),

  local container = $.core.v1.container,
  local envFrom = container.envFromType,

  oauth2_proxy_container::
    container.new('oauth2-proxy', $._images.oauth2_proxy) +
    container.withPorts($.core.v1.containerPort.new('http', 80)) +
    container.withArgs([
      '-http-address=0.0.0.0:80',
      '-redirect-url=%s' % $._config.oauth_redirect_url,
      '-upstream=%s' % $._config.oauth_upstream,
      '-email-domain=%s' % $._config.oauth_email_domain,
      '-pass-basic-auth=%s' % $._config.oauth_pass_basic_auth,
    ]) +
    container.withEnvFrom(
      envFrom.new() +
      envFrom.mixin.secretRef.withName('oauth2-proxy'),
    ),

  local deployment = $.apps.v1beta1.deployment,

  oauth2_proxy_deployment:
    deployment.new('oauth2-proxy', 1, [$.oauth2_proxy_container]),

  oauth2_proxy_service:
    $.util.serviceFor($.oauth2_proxy_deployment),
}