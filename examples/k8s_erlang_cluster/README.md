# Kubernetes Examples

An example showing the use of `gen_cluster` for creating an Erlang cluster in
Kubernetes with the 2 types of builtin DNS support, `A` and `SRV` records.

## IPs 

This build is done with the `rebar3` profile `ip_prod` and a separate Dockerfile
`Dockerfile.ip`.

### Setup
 
Under `examples/k8s_erlang_cluster` there is an example project that builds an
Erlang release with [kustomize](https://kustomize.io/) configuration to deploy a
cluster to Kubernetes. The example's `config/sys.config` contains configuration
for `gen_cluster` which is started on boot of the release:

```erlang
{discovery, {dns, #{domain => "k8s-erlang-cluster.k8s-erlang-cluster"}}}
```

This tells `gen_cluster` to use the DNS module for finding peers which defaults
to looking up `A` records and using the IPs as the host part of each node name.
The `domain` is set to `<service>.<namespace>` which resolves to records for
the service `k8s-erlang-cluster` in a namespace `k8s-erlang-cluster`.

To set the node name properly there are 3 pieces of configuration. First,
`config/vm.args.src` sets `-name` to use an environment variable `POD_IP`:

```shell
-name k8s_erlang_cluster@${NODE_HOST}
```

Next, the environment of the container that runs the release is made to set the
environment variable `POD_IP` to the IP address of that Pod in
`deployment/base/deployment.yaml`:

```yaml
env:
- name: ERL_DIST_PORT
  value: "39135"
- name: NODE_HOST
  valueFrom:
    fieldRef:
        fieldPath: status.podIP
```

> #### Note {: .info}
>
> `ERL_DIST_PORT` is also set in the environment. This tells the rebar3 release
> start script to configure the VM to listen on that port number for
> distribution connections, as well as what port to connect to other nodes
> on.  Setting the variable will disable EPMD as it is not needed if we know the
> port to use and the port used by all other nodes in the cluster.

Lastly, a [headless
service](https://kubernetes.io/docs/concepts/services-networking/service/#headless-services)
is used to expose the IPs of these Pods through DNS. A headless service is
created when `clusterIP` is set to `None` in the `Service` resource, found in
`deployment/base/service.yaml`:

```yaml
clusterIP: None
ports:
- protocol: TCP
  port: 39135
  targetPort: 39135
  name: dist-erl
```

A Service without `clusterIP: None` would have a single IP to load balance
requests to the Pods through. The headless service results in an Endpoint
per-Pod, see below in the steps to setup the cluster in
[kind](https://kind.sigs.k8s.io/) for an example.

### Run

The example can be installed to any Kubernetes cluster but comes with a
configuration (`examples/k8s_erlang_cluster/cluster.yaml`) for creating the
cluster with [kind](https://kind.sigs.k8s.io/) via the tool
[ctlptl](https://github.com/tilt-dev/ctlptl):

```shell
$ ctlptl apply -f cluster.yaml
```

The `cluster.yaml` configuration creates a kind cluster with a Docker registry
on localhost port 5005. This allows for the publishing of a Docker image for the
example release locally in a place the kind cluster can pull from:

```shell
$ docker build -f Dockerfile.ip -t localhost:5005/k8s_erlang_cluster:ip-0.1.0
$ docker push -f Dockerfile.ip localhost:5005/k8s_erlang_cluster:ip-0.1.0
```

The [kustomize](https://kustomize.io/) configuration can be used to install the
release with `kubectl apply -k`:

```shell
$ kubectl apply -k deployment/base
namespace/k8s-erlang-cluster created
configmap/k8s-erlang-cluster created
service/k8s-erlang-cluster created
deployment.apps/k8s-erlang-cluster created
```

With `get endpoints` we can view the results of the creation of a headless
Service that matches 3 Pods:

```shell
$ kubectl -n k8s-erlang-cluster get endpoints k8s-erlang-cluster
NAME                 ENDPOINTS                                            AGE
k8s-erlang-cluster   10.244.0.5:39135,10.244.0.6:39135,10.244.0.7:39135   7m11s
```

To see that the cluster is formed run a command against a single Pod in the
Deployment with `exec`:

```shell
$ kubectl exec -n k8s-erlang-cluster deploy/k8s-erlang-cluster -- bin/k8s_erlang_cluster eval 'nodes().'
['k8s_erlang_cluster@10.244.0.5', 'k8s_erlang_cluster@10.244.0.7']
```

## SRV

This section builds on the previous section on using [IPs](#ips) for the host
name part of a node name and DNS queries for A records. The necessary changes to
the Kubernetes deployment are done in a kustomize overlay,
`deployment/overlays/srv`.

The SRV specific build is done with the `rebar3` profile `srv_prod` and a
separate Dockerfile `Dockerfile.srv`. It also includes a necessary change to the
CoreDNS ConfigMap in the Kubernetes cluster.

<a name="setup-srv"></a>
### Setup

To us `SRV` records we set `record_type` to `srv` in the `gen_cluster`
configuration. A separate `sys.config` file is used by the `rebar3` profile
named `srv_sys.config`:

```erlang
[{discovery, {dns, #{record_type => srv,
                     domain => "_dist-erl._tcp.k8s-erlang-cluster.k8s-erlang-cluster"}}}]},
```

This tells `gen_cluster` to use the DNS module for finding peers by looking up
the `SRV` records and using the results as the host part of each node name.  The
`domain` is set to `_<port name>._<protocol>.<service>.<namespace>` which
resolves to records for the service `k8s-erlang-cluster` in a namespace
`k8s-erlang-cluster` for the port named `dist-erl` and using protocol `TCP`.
This is setup in the `base/service.yaml` the same as is used for the `IP`
version of discovery above, but made use of here:

```yaml
kind: Service
apiVersion: v1
metadata:
  name: k8s-erlang-cluster
spec:
  clusterIP: None
  ports:
  - protocol: TCP
    port: 39135
    targetPort: dist-erl
    name: dist-erl
```

A separate `vm.args` is used because we want to set no host part of the node
name after `-name`:

```shell
-name k8s_erlang_cluster
```

This way Erlang essentially uses the result of the command `hostname --fqdn`. By
default in Kubernetes this will not include any Service subdomain and the FQDN
will be the same as the short name. So `subdoman` must be added to the
Pod spec of the Deployment resource:

```yaml
subdomain: "k8s-erlang-cluster"
```

This is done in a kustomize patch file added to
`deployment/overlays/srv/kustomization.yaml`:

```yaml
patches:
- path: deployment.yaml
```

Now `hostname --fqdn` on the container in the Pod would result in `<pod
name>.k8s-erlang-cluster.svc.cluster.local`

Nothing is changed in the `env` of `deployment.yaml` but the only part used in
this container is the `ERL_DIST_PORT`:

```yaml
env:
- name: ERL_DIST_PORT
  value: "39135"
```

While the same [headless
service](https://kubernetes.io/docs/concepts/services-networking/service/#headless-services)
is used to expose the IPs of these Pods through DNS, by default the DNS records
for a `SRV` query would look like:

```
_dist-erl._tcp.k8s-erlang-cluster.k8s-erlang-cluster.svc.cluster.local  service
= 0 33 39135 10-244-0-15.k8s-erlang-cluster.k8s-erlang-cluster.svc.cluster.local.
```

Notice the `10-244-0-15` part of the name. This is the IP of the corresponding
Pod. Using the Pod name instead of the IP with A records or the IP based domain
could be more useful in logs and other telemetry about a node. Also note that,
as far as I've ever found, there is no way to actually get the IP based name
easily into an environment variable to use as a name in the Erlang node name.

In order for DNS to use the name of the Pod we can add an option to the
configuration of CoreDNS through its ConfigMap:

```
$ kubectl -n kube-system edit configmap coredns
...
kubernetes cluster.local in-addr.arpa ip6.arpa {
    pods insecure
    fallthrough in-addr.arpa ip6.arpa
    ttl 30
    endpoint_pod_names
}
...
$ kubectl -n kube-system rollout restart deployment coredns
```

This will restart the Pods in the CoreDNS Deployment and pickup the modified
ConfigMap with `endpoint_pod_names` set.

In the example a ConfigMap resource is provided
(`deployments/overlays/srv/coredns-configmap.yaml`) and applied with `kubectl`,
but for CoreDNS to pickup the configuration change the deployment must still be
restarted:

```shell
$ kubectl -n kube-system rollout restart deployment coredns
```

I first learned about this CoreDNS option from the blog posts [K8s Erlang
Clustering](https://lrascao.github.io/k8s-erlang-clustering/) by [Luis
Rascão](https://github.com/lrascao) and [CoreDNS:
`endpoint_pod_names`](https://blog.differentpla.net/blog/2022/01/08/coredns-endpoint-pod-names/)
by [Roger Lipscombe](http://rogerlipscombe.me/).

### Run

The example can be installed to any Kubernetes cluster but comes with a
configuration (`examples/k8s_erlang_cluster/cluster.yaml`) for creating the
cluster with [kind](https://kind.sigs.k8s.io/) via the tool
[ctlptl](https://github.com/tilt-dev/ctlptl):

```shell
$ ctlptl apply -f cluster.yaml
```

The `cluster.yaml` configuration creates a kind cluster with a Docker registry
on localhost port 5005. This allows for the publishing of a Docker image for the
example release locally in a place the kind cluster can pull from:

```shell
$ docker build -f Dockerfile.srv -t localhost:5005/k8s_erlang_cluster:srv-0.1.0
$ docker push -f Dockerfile.srv localhost:5005/k8s_erlang_cluster:srv-0.1.0
```

The [kustomize](https://kustomize.io/) `srv` overlay is used to install the
release with `kubectl apply -k`:

```shell
$ kubectl apply -k deployment/overlays/srv
namespace/k8s-erlang-cluster created
Warning: resource configmaps/coredns is missing the kubectl.kubernetes.io/last-applied-configuration annotation which is required by kubectl apply. kubectl apply should only be used on resources created declaratively by either kubectl create --save-config or kubectl apply. The missing annotation will be patched automatically.
configmap/coredns configured
service/k8s-erlang-cluster created
deployment.apps/k8s-erlang-cluster created
```

CoreDNS must now be manually restarted to pickup the new configuration:

```shell
$ kubectl rollout restart -n kube-system deployment coredns
```

It may take a few seconds for the cluster to form because we had to restart DNS
and wait for a `gen_cluster` refresh. To see that the cluster has formed and the
host part of the node name is an FQDN of `<pod
name>.<service>.<namespace>.svc.cluster.local` run a command against a single
Pod in the Deployment with `exec`:

```shell
$ kubectl exec -n k8s-erlang-cluster deploy/k8s-erlang-cluster -- bin/k8s_erlang_cluster eval 'nodes().'
['k8s_erlang_cluster@k8s-erlang-cluster-5f4954c6b5-kzcwl.k8s-erlang-cluster.k8s-erlang-cluster.svc.cluster.local', 'k8s_erlang_cluster@k8s-erlang-cluster-5f4954c6b5-6ts2w.k8s-erlang-cluster.k8s-erlang-cluster.svc.cluster.local']
```

## StatefulSet 

Another option for host names that work with SRV records is a
[StatefulSet](https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/).
These provide a stable host name for each Pod `<statefulset name>-<ordinal>`.
The same steps in the [SRV](#setup-srv) section will work to configure the release and
discovery with the key difference that no configuration changes to CoreDNS are
necessary for this to work.

But there are limitations to use of a StatefulSet and they should only be used
where you truly need the stable network name or persistent storage. See the
Kubernetes docs for more on
[StatefulSets](https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/).
