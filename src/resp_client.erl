%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(resp_client).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([send/1]).
-export([start/0]).
-export([start/1]).
-export([start_link/0]).
-export([start_link/1]).
-import(resp_statem, [nei/1]).
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").


start() ->
    ?FUNCTION_NAME(#{}).


start(Arg) ->
    gen_statem:start(?MODULE, [Arg], []).


start_link() ->
    ?FUNCTION_NAME(#{}).


start_link(Arg) ->
    gen_statem:start_link(?MODULE, [Arg], envy_gen:options(?MODULE)).


send(#{data := Data} = Arg) ->
    send_request(
      maps:without(
        [data],
        Arg#{request => {?FUNCTION_NAME, Data}})).


send_request(#{label := _} = Arg) ->
    resp_statem:send_request(Arg);

send_request(Arg) ->
    resp_statem:send_request(Arg#{label => ?MODULE}).


init([Arg]) ->
    process_flag(trap_exit, true),
    {ok, disconnected, Arg#{requests => gen_statem:reqids_new()}}.


callback_mode() ->
    handle_event_function.


handle_event({call, _}, {send, _}, disconnected, Data) ->
    {next_state, connecting, Data, [postpone, nei(open)]};

handle_event({call, _}, {send, _}, connecting, _) ->
    {keep_state_and_data, postpone};

handle_event({call, _}, {send, _}, {busy, _}, _) ->
    {keep_state_and_data, postpone};

handle_event({call, From}, {send, _} = Send, connected, Data) ->
    {next_state,
     {busy, From},
     Data#{replies => []},
     nei(Send)};

handle_event(internal, open = EventName, connecting, Data) ->
    case socket:open(inet, stream, default) of
        {ok, Socket} ->
            {keep_state,
             Data#{socket => Socket},
             [nei({telemetry, EventName, #{count => 1}}),
              nei(connect)]};

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(internal, {send, #{} = Decoded}, {busy, _}, _) ->
    {keep_state_and_data, nei({send, [Decoded]})};

handle_event(internal,
             {send = EventName, Decoded},
             {busy, _},
             #{socket := Socket}) ->
    Encoded = resp_codec:encode(Decoded),
    case socket:send(Socket, Encoded) of
        ok ->
            {keep_state_and_data,
             [nei({telemetry,
                   EventName,
                   #{count => 1,
                     bytes => iolist_size(Encoded)},
                   #{messages => Decoded}}),
              nei({reply_expected, resp_protocol:reply_expected(Decoded)})]};

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(internal, {reply_expected, []}, {busy, From}, Data) ->
    {next_state,
     connected,
     maps:without([replies], Data),
     {reply, From, ok}};

handle_event(internal, {reply_expected, ReplyExpected}, {busy, _}, Data) ->
    {keep_state, Data#{reply_expected => ReplyExpected}};

handle_event(internal,
             recv,
             _,
             #{socket := Socket, partial := Partial} = Data) ->
    case socket:recv(Socket, 0, nowait) of
        {ok, Received} ->
            {keep_state,
             Data#{partial := <<>>},
             [nei({telemetry, recv, #{bytes => iolist_size(Received)}}),
              nei({recv, iolist_to_binary([Partial, Received])}),
              nei(recv)]};

        {select, {select_info, _, _}} ->
            keep_state_and_data;

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(info,
             {'$socket', Socket, select, Handle},
             _,
             #{socket := Socket, partial := Partial} = Data) ->
    case socket:recv(Socket, 0, Handle) of
        {ok, Received} ->
            {keep_state,
             Data#{partial := <<>>},
             [nei({telemetry, recv, #{bytes => iolist_size(Received)}}),
              nei({recv, iolist_to_binary([Partial, Received])}),
              nei(recv)]};

        {select, {select_info, _, _}} ->
            keep_state_and_data;

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             nei({response, #{label => Label, reply => Reply}})};

        {{error, {Reason, ServerRef}}, Label, UpdatedRequests} ->
                {stop,
                 #{reason => Reason,
                   server_ref => ServerRef,
                   label => Label},
                 Data#{requests := UpdatedRequests}}
    end;

handle_event(internal, {recv, Command}, _, _) ->
    {keep_state_and_data, nei({decode, Command})};

handle_event(internal, {decode, <<>>}, _, _) ->
    keep_state_and_data;

handle_event(internal, {decode, Encoded}, _, #{partial := <<>>} = Data) ->
    case resp_codec:decode(Encoded) of
        {Decoded, Remainder} ->
            {keep_state_and_data,
             [nei({message, Decoded}), nei({decode, Remainder})]};

        partial ->
            {keep_state, Data#{partial := Encoded}}
    end;

handle_event(internal,
             {message, Decoded},
             {busy, From},
             #{replies := Replies} = Data) ->
    {next_state,
     connected,
     maps:without([replies, reply_expected], Data),
     [{reply, From, lists:reverse([Decoded | Replies])},
      nei({telemetry, message, #{count => 1}, #{message => Decoded}})]};

handle_event(internal,
             {telemetry, EventName, Measurements},
             _,
             _) ->
    {keep_state_and_data,
     nei({telemetry, EventName, Measurements, #{}})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             Data) ->
    ok = telemetry:execute(
           [resp, client, EventName],
           Measurements,
           maps:merge(
             maps:with([socket], Data),
             Metadata)),
    keep_state_and_data;

handle_event(internal, connect, connecting, _) ->
    {keep_state_and_data,
     nei({connect,
          #{family => inet,
            port => resp_config:client(port),
            addr => addr()}})};

handle_event(internal,
             {connect = EventName, Arg},
             connecting,
             #{socket := Socket} = Data) ->
    case socket:connect(Socket, Arg) of
        ok ->
            {next_state,
             connected,
             Data#{partial => <<>>},
             [nei({telemetry, EventName, #{count => 1}, Arg}),
              nei(recv)]};

        {error, Reason} ->
            {stop, Reason}
    end.


addr() ->
    ?FUNCTION_NAME(resp_config:client(hostname)).


addr(Hostname) ->
    {ok, #hostent{h_addr_list = Addresses}} = inet:gethostbyname(Hostname),
    pick_one(Addresses).


pick_one(L) ->
    lists:nth(rand:uniform(length(L)), L).
