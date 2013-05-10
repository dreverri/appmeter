Appmeter allows Erlang applications to output metric information via handlers
and provides a sampling mechanism to prevent metric collection/output from
negatively impacting performance.

## Usage

```erlang

application:start(appmeter).
{ok, Pid} = appmeter:proxy().
appmeter:send_one(Pid, {measure, <<"foo">>, 1}).
appmeter:send_many(Pid, [{count, <<"bar">>, 3}, {gauge, <<"baz">>, 9}]).

```

## Sampling

Sampling is done by a proxy process. Proxy processes are intended to be long
lived processes that stand next to some application process producing metric
data. For example, in Riak you might pair a proxy process with each vnode
process.

Sampling is controlled by two parameters, `flush_interval` and
`max_sample_size`. Between any two flushes a proxy process will buffer at most
`max_sample_size` metrics. A count is maintained of all observed metrics in
order to calculate a sample rate.

### Flush Process

To avoid overwhelming any process message queues and to provide back pressure a
seperate process per proxy is used to shuffle messages from the proxy to the
event manager. This "flusher" process periodically drains sampled metrics from
the proxy process and sends them along to the event manager. The flusher uses a
synchronous call to the event manager to prevent overwhelming the event manager
which is handling messages from many flusher processes.

The proxy process does not communicate directly with the event manager to avoid
generating large message queues. If the proxy process communicated directly with the event manager using a synchronous call, the proxy process's message queue could
be overwhelmed. If the proxy process used an asynchronous call to the event
manager, the event manager's message queue could be overwhelmed.

## Metrics

Three metric types are recognized `count`, `gauge`, and `measure`.

## Handlers

StatsD is currently the only handler that has been implemented.

## Supervision Tree

```
appmeter_sup (one_for_one)
  - appmeter_event_mgr_sup (one_for_one)
    - appmeter_event_mgr
  - appmeter_proxy_sup (simple_one_for_one)
    - appmeter_proxy
```

Proxy processes are supervised by a simple_one_for_one supervisor, however, the
processes are configured as transient which means they will not be restarted.
Additionally, sending data to a proxy process uses a gen_server:cast/2 which
means that if the proxy process dies the send operation will fail silently. The
proxy process should monitored and corrective action should be taken when
necessary (e.g. start a new proxy process).
