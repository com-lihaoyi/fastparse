// Override defaults paramters for objects in the ksonnet libs here.
local k = import 'k.libsonnet';

k {
  _config+:: {
    enable_rbac: true,
    namespace: error 'Must define a namespace',
  },

  core+: {
    v1+: {
      configMap+: {
        new(name)::
          super.new(name, {}),
      },

      // Expose containerPort type.
      containerPort:: $.core.v1.container.portsType {
        // Force all ports to have names.
        new(name, port)::
          super.newNamed(name, port),

        // Shortcut constructor for UDP ports.
        newUDP(name, port)::
          super.newNamed(name, port) +
          super.withProtocol('UDP'),
      },

      // Expose volumes type.
      volume:: $.core.v1.pod.mixin.spec.volumesType {
        // Remove items parameter from fromConfigMap
        fromConfigMap(name, configMapName)::
          super.withName(name) +
          super.mixin.configMap.withName(configMapName),

        // Shortcut constructor for secret volumes.
        fromSecret(name, secret)::
          super.withName(name) +
          super.mixin.secret.withSecretName(secret),
      },

      volumeMount:: $.core.v1.container.volumeMountsType {
        // Override new, such that it doesn't always set readOnly: false.
        new(name, mountPath, readOnly=false)::
          {} + self.withName(name) + self.withMountPath(mountPath) +
          if readOnly
          then self.withReadOnly(readOnly)
          else {},
      },

      persistentVolumeClaim+:: {
        new():: {},
      },

      container:: $.extensions.v1beta1.deployment.mixin.spec.template.spec.containersType {
        new(name, image)::
          super.new(name, image) +
          super.withImagePullPolicy('IfNotPresent'),

        withEnvMixin(es)::
          // if an envvar has an empty value ("") we want to remove that property
          // because k8s will remove that and then it would always
          // show up as a difference.
          local removeEmptyValue(obj) =
            if std.objectHas(obj, 'value') && std.length(obj.value) == 0 then
              {
                [k]: obj[k]
                for k in std.objectFields(obj)
                if k != 'value'
              }
            else
              obj;
          super.withEnvMixin([
            removeEmptyValue(envvar)
            for envvar in es
          ]),

        withEnvMap(es)::
          self.withEnvMixin([
            $.core.v1.container.envType.new(k, es[k])
            for k in std.objectFields(es)
          ]),
      },

      toleration:: $.extensions.v1beta1.deployment.mixin.spec.template.spec.tolerationsType,

      servicePort:: $.core.v1.service.mixin.spec.portsType,
    },
  },

  local appsExtentions = {
    daemonSet+: {
      new(name, containers)::
        super.new() +
        super.mixin.metadata.withName(name) +
        super.mixin.spec.template.metadata.withLabels({ name: name }) +
        super.mixin.spec.template.spec.withContainers(containers) +

        // Can't think of a reason we wouldn't want a DaemonSet to run on
        // every node.
        super.mixin.spec.template.spec.withTolerations([
          $.core.v1.toleration.new() +
          $.core.v1.toleration.withOperator('Exists') +
          $.core.v1.toleration.withEffect('NoSchedule'),
        ]) +

        // We want to specify a minReadySeconds on every deamonset, so we get some
        // very basic canarying, for instance, with bad arguments.
        super.mixin.spec.withMinReadySeconds(10) +
        super.mixin.spec.updateStrategy.withType('RollingUpdate'),
    },

    deployment+: {
      new(name, replicas, containers, podLabels={})::
        super.new(name, replicas, containers, podLabels { name: name }) +

        // We want to specify a minReadySeconds on every deployment, so we get some
        // very basic canarying, for instance, with bad arguments.
        super.mixin.spec.withMinReadySeconds(10) +

        // We want to add a sensible default for the number of old deployments
        // handing around.
        super.mixin.spec.withRevisionHistoryLimit(10),
    },

    statefulSet+: {
      new(name, replicas, containers, volumeClaims, podLabels={})::
        super.new(name, replicas, containers, volumeClaims, podLabels { name: name }) +
        super.mixin.spec.updateStrategy.withType("RollingUpdate"),
    },
  },

  extensions+: {
    v1beta1+: appsExtentions,
  },

  apps+: {
    v1beta1+: appsExtentions,
  },

  rbac+: {
    v1beta1+: {
      // Shortcut to access the hidden types.
      policyRule:: $.rbac.v1beta1.clusterRole.rulesType,

      subject:: $.rbac.v1beta1.clusterRoleBinding.subjectsType {
        withKind(kind):: self + { kind: kind },
      },

      roleBinding+: {
        mixin+: {
          roleRef+: {
            withKind(kind):: self.mixinInstance({ kind: kind }),
          },
        },
      },

      clusterRoleBinding+: {
        mixin+: {
          roleRef+: {
            withKind(kind):: self.mixinInstance({ kind: kind }),
          },
        },
      },
    },
  },

  util+:: {
    // serviceFor create service for a given deployment.
    serviceFor(deployment)::
      local container = $.core.v1.container;
      local servicePort = $.core.v1.service.mixin.spec.portsType;
      local ports = [
        servicePort.newNamed(c.name + '-' + port.name, port.containerPort, port.containerPort) +
        if std.objectHas(port, 'protocol')
        then servicePort.withProtocol(port.protocol)
        else {}
        for c in deployment.spec.template.spec.containers
        for port in (c + container.withPortsMixin([])).ports
      ];
      $.core.v1.service.new(
        deployment.metadata.name,  // name
        deployment.spec.template.metadata.labels,  // selector
        ports,
      ),

    // rbac creates a service account, role and role binding with the given
    // name and rules.
    rbac(name, rules)::
      if $._config.enable_rbac
      then {
        local clusterRole = $.rbac.v1beta1.clusterRole,
        local clusterRoleBinding = $.rbac.v1beta1.clusterRoleBinding,
        local subject = $.rbac.v1beta1.subject,
        local serviceAccount = $.core.v1.serviceAccount,

        service_account:
          serviceAccount.new(name),

        cluster_role:
          clusterRole.new() +
          clusterRole.mixin.metadata.withName(name) +
          clusterRole.withRules(rules),

        cluster_role_binding:
          clusterRoleBinding.new() +
          clusterRoleBinding.mixin.metadata.withName(name) +
          clusterRoleBinding.mixin.roleRef.withApiGroup('rbac.authorization.k8s.io') +
          clusterRoleBinding.mixin.roleRef.withKind('ClusterRole') +
          clusterRoleBinding.mixin.roleRef.withName(name) +
          clusterRoleBinding.withSubjects([
            subject.new() +
            subject.withKind('ServiceAccount') +
            subject.withName(name) +
            subject.withNamespace($._config.namespace),
          ]),
      }
      else {},

    namespacedRBAC(name, rules)::
      if $._config.enable_rbac
      then {
        local role = $.rbac.v1beta1.role,
        local roleBinding = $.rbac.v1beta1.roleBinding,
        local subject = $.rbac.v1beta1.subject,
        local serviceAccount = $.core.v1.serviceAccount,

        service_account:
          serviceAccount.new(name) +
          serviceAccount.mixin.metadata.withNamespace($._config.namespace),

        role:
          role.new() +
          role.mixin.metadata.withName(name) +
          role.mixin.metadata.withNamespace($._config.namespace) +
          role.withRules(rules),

        cluster_role_binding:
          roleBinding.new() +
          roleBinding.mixin.metadata.withName(name) +
          roleBinding.mixin.metadata.withNamespace($._config.namespace) +
          roleBinding.mixin.roleRef.withApiGroup('rbac.authorization.k8s.io') +
          roleBinding.mixin.roleRef.withKind('Role') +
          roleBinding.mixin.roleRef.withName(name) +
          roleBinding.withSubjects([
            subject.new() +
            subject.withKind('ServiceAccount') +
            subject.withName(name) +
            subject.withNamespace($._config.namespace),
          ]),
      }
      else {},

    // VolumeMount helper functions can be augmented with mixins.
    // For example, passing "volumeMount.withSubPath(subpath)" will result in
    // a subpath mixin.
    configVolumeMount(name, path, volumeMountMixin={})::
      local container = $.core.v1.container,
            deployment = $.extensions.v1beta1.deployment,
            volumeMount = $.core.v1.volumeMount,
            volume = $.core.v1.volume,
            addMount(c) = c + container.withVolumeMountsMixin(
        volumeMount.new(name, path) +
        volumeMountMixin,
      );

      deployment.mapContainers(addMount) +
      deployment.mixin.spec.template.spec.withVolumesMixin([
        volume.fromConfigMap(name, name),
      ]),

    hostVolumeMount(name, hostPath, path, readOnly=false, volumeMountMixin={})::
      local container = $.core.v1.container,
            deployment = $.extensions.v1beta1.deployment,
            volumeMount = $.core.v1.volumeMount,
            volume = $.core.v1.volume,
            addMount(c) = c + container.withVolumeMountsMixin(
        volumeMount.new(name, path, readOnly=readOnly) +
        volumeMountMixin,
      );

      deployment.mapContainers(addMount) +
      deployment.mixin.spec.template.spec.withVolumesMixin([
        volume.fromHostPath(name, hostPath),
      ]),

    secretVolumeMount(name, path, defaultMode=256, volumeMountMixin={})::
      local container = $.core.v1.container,
            deployment = $.extensions.v1beta1.deployment,
            volumeMount = $.core.v1.volumeMount,
            volume = $.core.v1.volume,
            addMount(c) = c + container.withVolumeMountsMixin(
        volumeMount.new(name, path) +
        volumeMountMixin,
      );

      deployment.mapContainers(addMount) +
      deployment.mixin.spec.template.spec.withVolumesMixin([
        volume.fromSecret(name, name) +
        volume.mixin.secret.withDefaultMode(defaultMode),
      ]),

    emptyVolumeMount(name, path, volumeMountMixin={})::
      local container = $.core.v1.container,
            deployment = $.extensions.v1beta1.deployment,
            volumeMount = $.core.v1.volumeMount,
            volume = $.core.v1.volume,
            addMount(c) = c + container.withVolumeMountsMixin(
        volumeMount.new(name, path) +
        volumeMountMixin,
      );

      deployment.mapContainers(addMount) +
      deployment.mixin.spec.template.spec.withVolumesMixin([
        volume.fromEmptyDir(name),
      ]),

    manifestYaml(value):: (
      local f = std.native('manifestYamlFromJson');
      f(std.toString(value))
    ),

    resourcesRequests(cpu, memory)::
      $.core.v1.container.mixin.resources.withRequests({
        cpu: cpu,
        memory: memory,
      }),

    resourcesLimits(cpu, memory)::
      $.core.v1.container.mixin.resources.withLimits({
        cpu: cpu,
        memory: memory,
      }),

    antiAffinity:
      {
        local deployment = $.apps.v1beta1.deployment,
        local podAntiAffinity = deployment.mixin.spec.template.spec.affinity.podAntiAffinity,
        local name = super.spec.template.metadata.labels.name,

        spec+: podAntiAffinity.withRequiredDuringSchedulingIgnoredDuringExecution([
          podAntiAffinity.requiredDuringSchedulingIgnoredDuringExecutionType.new() +
          podAntiAffinity.requiredDuringSchedulingIgnoredDuringExecutionType.mixin.labelSelector.withMatchLabels({ name: name }) +
          podAntiAffinity.requiredDuringSchedulingIgnoredDuringExecutionType.withTopologyKey('kubernetes.io/hostname'),
        ]).spec,
      },
  },
}