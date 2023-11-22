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


-module(resp_connection).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([start/0]).
-export([start/1]).
-export([start_link/0]).
-export([start_link/1]).
-export([terminate/3]).
-import(resp_statem, [nei/1]).
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").


start() ->
    ?FUNCTION_NAME(#{}).


start(Arg) ->
    gen_statem:start(?MODULE, [Arg], envy_gen:options(?MODULE)).


start_link() ->
    ?FUNCTION_NAME(#{}).


start_link(Arg) ->
    gen_statem:start_link(?MODULE, [Arg], envy_gen:options(?MODULE)).


init([Arg]) ->
    process_flag(trap_exit, true),
    {ok,
     ready,
     #{arg => Arg,
       partial => <<>>,
       requests => gen_statem:reqids_new()},
     nei(recv)}.


callback_mode() ->
    handle_event_function.


handle_event(
  internal,
  {callback, F, A},
  _,
  #{arg := #{callback := #{module := M} = Callback} = Arg} = Data) ->
    try apply(M, F, A) of
        {continue, CallbackData, Actions} when is_list(Actions) ->
            {keep_state,
             Data#{arg := Arg#{callback := Callback#{data := CallbackData}}},
             [nei(Action) || Action <- Actions]};

        {continue, CallbackData, Action} ->
            {keep_state,
             Data#{arg := Arg#{callback := Callback#{data := CallbackData}}},
             nei(Action)};

        {continue, Actions} when is_list(Actions) ->
            {keep_state_and_data, [nei(Action) || Action <- Actions]};

        {continue, Action} ->
            {keep_state_and_data, nei(Action)};

        continue ->
            keep_state_and_data;

        stop ->
            stop;

        {stop, Reason} ->
            {stop, Reason}
    catch
        Class:Exception:Stacktrace ->
            ?LOG_ERROR(#{class => Class,
                         exception => Exception,
                         stacktrace => Stacktrace}),
            {keep_state_and_data,
             nei({encode, {error, "not implemented"}})}
    end;

handle_event(info,
             {'$socket', Socket, select, Handle},
             _,
             #{arg := #{socket := Socket},
               partial := Partial} = Data) ->
    case socket:recv(Socket, 0, Handle) of
        {ok, Received} ->
            {keep_state,
             Data#{partial := <<>>},
             [nei({telemetry,
                   recv,
                   #{bytes => iolist_size(Received)}}),
              nei({recv, iolist_to_binary([Partial, Received])}),
              nei(recv)]};

        {select, {select_info, _, _}} ->
            keep_state_and_data;

        {error, econnreset} ->
            stop;

        {error, closed} ->
            stop;

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(internal,
             {recv, <<_:8, _/bytes>>= Encoded},
             _,
             #{arg := #{callback := #{data := CallbackData}}}) ->
    case resp_codec:decode(Encoded) of
        {{array, [{bulk, Command} | _]} = Decoded, Remainder} ->
            try
                {keep_state_and_data,
                 [nei({callback,
                       recv,
                       [#{message => Decoded,
                          command => command(string:casefold(Command)),
                          data => CallbackData}]}),

                  nei({telemetry,
                       decode,
                       #{count => 1},
                       #{message => Decoded}}),

                  nei({recv, Remainder})]}

            catch
                error:badarg ->
                    {keep_state_and_data,
                     [nei({encode, {error, ["unknown command '", Command, "'"]}}),
                      nei({recv, Remainder})]}
            end;

        {_, Remainder} ->
            {keep_state_and_data,
             [nei({encode, {error, ["syntax"]}}), nei({recv, Remainder})]}
    end;

handle_event(internal, {recv, <<>>}, _, _) ->
    keep_state_and_data;

handle_event(internal, {encode = EventName, Decoded}, _, _) ->
    {keep_state_and_data,
     [nei({telemetry,
           EventName,
           #{count => 1},
           #{message => Decoded}}),
      nei({send, resp_codec:encode(Decoded)})]};

handle_event(internal,
             {send = EventName, Encoded},
             _,
             #{arg := #{socket := Socket}}) ->
    case socket:send(Socket, Encoded) of
        ok ->
            {keep_state_and_data,
             nei({telemetry,
                  EventName,
                  #{bytes => iolist_size(Encoded)}})};

        {error, econnreset} ->
            stop;

        {error, closed} ->
            stop;

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(internal,
             recv = EventName,
             _,
             #{arg := #{socket := Socket},
               partial := Partial} = Data) ->
    case socket:recv(Socket, 0, nowait) of
        {ok, Received} ->
            {keep_state,
             Data#{partial := <<>>},
             [nei({telemetry,
                   EventName,
                   #{bytes => iolist_size(Received)}}),
              nei({recv, iolist_to_binary([Partial, Received])}),
              nei(recv)]};

        {select, {select_info, _, _}} ->
            keep_state_and_data;

        {error, econnreset} ->
            stop;

        {error, closed} ->
            stop;

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(internal,
             {telemetry, EventName, Measurements},
             _,
             _) ->
    {keep_state_and_data,
     nei({telemetry, EventName, Measurements, #{}})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             #{arg := #{callback := #{module := M}}}) ->
    ok = telemetry:execute(
           [resp, connection, EventName],
           Measurements,
           maps:merge(
             #{callback => M},
             Metadata)),
    keep_state_and_data;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, ok}, expire, Updated} ->
            {keep_state, Data#{requests := Updated}};

        {{reply, ok}, flush_all, Updated} ->
            {keep_state, Data#{requests := Updated}};

        no_request ->
            {keep_state_and_data, nei({callback, info, [Msg]})};

        no_reply ->
            ?LOG_ERROR(#{msg => Msg, data => Data}),
            keep_state_and_data
    end.


terminate(_Reason, _State, #{arg := #{socket := Socket}}) ->
    _ = socket:close(Socket);

terminate(_Reason, _State, _Data) ->
    ok.


command(Command) ->
    #{name => binary_to_atom(Command),
      info => resp_command:info(string:casefold(Command))}.
