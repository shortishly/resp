# Erlang Redis Serialization Protocol Server (and Client)

## Protocol

Encoding and decoding of bulk, array, string, error and integer types
are in [resp_codec](src/resp_codec.erl).

```erlang
3> resp_codec:decode(<<"*2\r\n$5\r\nhello\r\n$5\r\nworld\r\n">>).

{{array, [{bulk,<<"hello">>}, {bulk,<<"world">>}]}, <<>>}
```

The returned value is a tuple of decoded protocol, with any remaining
bytes as the second element.

```erlang
resp_codec:encode({array, [{bulk, <<"hello">>}, {bulk, <<"world">>}]}).

[42,"2",<<"\r\n">>,
 [[36,<<"5">>,<<"\r\n">>,<<"hello">>,<<"\r\n">>],
  [36,<<"5">>,<<"\r\n">>,<<"world">>,<<"\r\n">>]]]
```

## Server

The bare bones of a Redis Serialization Protocol server implementing a
fraction of the available commands:

- INFO
- HSET
- HGET
- HGETALL

## Client

Start the client from the command line:

```erlang
{ok, C} = resp_client:start().
```

### HSET


```erlang
gen_statem:receive_response(
  resp_client:send(
    #{server_ref => C,
      data => {array, [{bulk, "HSET"},
                       {bulk, "user:123"},
                       {bulk, "username"},
                       {bulk, "martina"},
                       {bulk, "firstName"},
                       {bulk, "Martina"},
                       {bulk, "lastName"},
                       {bulk, "Elisa"},
                       {bulk, "country"},
                       {bulk, "GB"}]}})).

{reply,[{integer,4}]}
```

## Telemetry

Both the client and server have instrumentation using Telemetry.
