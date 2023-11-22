<br>

<p align="center">
    <a href="https://shortishly.github.io/resp/cover/">
      <img alt="Test Coverage" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fresp%2Fcover%2Fcoverage.json&query=%24.total&suffix=%25&style=flat-square&label=Test%20Coverage&color=green">
    </a>
    <a href="https://shortishly.github.io/resp/edoc/">
      <img alt="edoc" src="https://img.shields.io/badge/Documentation-edoc-green?style=flat-square">
    </a>
    <a href="https://erlang.org/">
      <img alt="Erlang/OTP 25+" src="https://img.shields.io/badge/Erlang%2FOTP-25%2B-green?style=flat-square">
    </a>
    <a href="https://www.apache.org/licenses/LICENSE-2.0">
      <img alt="Apache-2.0" src="https://img.shields.io/github/license/shortishly/resp?style=flat-square">
    </a>
</p>

## What is RESP?

[resp][resp-github] implements both the server and client of Redis
serialization protocol in Erlang.

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

[resp-github]: https://github.com/shortishly/resp
