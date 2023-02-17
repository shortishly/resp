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


-module(resp_listener).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
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
    gen_statem:start(?MODULE, [Arg], envy_gen:options(?MODULE)).


start_link() ->
    ?FUNCTION_NAME(#{}).


start_link(Arg) ->
    gen_statem:start_link(?MODULE, [Arg], envy_gen:options(?MODULE)).


init([Arg]) ->
    process_flag(trap_exit, true),
    case resp_config:enabled(listener) of
        true ->
            {ok,
             unready,
             #{arg => Arg, requests => gen_statem:reqids_new()},
             [nei(callback_init), nei(open)]};

        false ->
            ignore
    end.


callback_mode() ->
    handle_event_function.


handle_event(internal,
             callback_init,
             _,
             #{arg := #{callback := Module}} = Data) ->
    case Module:init([]) of
        {ok, CallbackData} ->
            {keep_state, Data#{callback_data => CallbackData}};

        stop ->
            stop;

        {stop, Reason} ->
            {stop, Reason}
    end;

handle_event(internal,
             {connect = EventName, Socket},
             _,
             #{callback_data := CallbackData,
               arg := #{callback := Module}}) ->
    {ok, Child} = resp_connection_sup:start_child(
                    #{socket => Socket,
                      callback => #{data => CallbackData,
                                    module => Module}}),
    {keep_state_and_data,
     [nei({setopt,
           #{socket => Socket,
             level => otp,
             option => controlling_process,
             value => Child}}),

      nei({telemetry, EventName, #{count => 1}})]};

handle_event(info,
             {'$socket', Listener, select = EventName, Handle},
             _,
             #{socket := Listener}) ->
    case socket:accept(Listener, Handle) of
        {ok, Connected} ->
            {keep_state_and_data,
             [nei({telemetry, EventName, #{count => 1}}),
              nei({connect, Connected}),
              nei(accept)]};

        {select, {select_info, _, _}} ->
            keep_state_and_data;

        {error, Reason} ->
            {stop, Reason}
    end;


handle_event(internal, accept = EventName, _, #{socket := Listener}) ->
    case socket:accept(Listener, nowait) of
        {ok, Connected} ->
            {keep_state_and_data,
             [nei({telemetry, EventName, #{count => 1}}),
              nei({connect, Connected}),
              nei(accept)]};

        {select, {select_info, _, _}} ->
            keep_state_and_data;

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(internal, listen = EventName, _, #{socket := Listener}) ->
    case socket:listen(Listener, resp_config:listener(backlog)) of
        ok ->
            {keep_state_and_data,
             [nei({telemetry, EventName, #{count => 1}}), nei(accept)]};

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(internal, open = EventName, _, Data) ->
    case socket:open(inet, stream, tcp) of
        {ok, Listener} ->
            {keep_state,
             Data#{socket => Listener},
             [nei({telemetry,
                   EventName,
                   #{count => 1}}),

              nei({setopt,
                   #{socket => Listener,
                     level => socket,
                     option => reuseaddr,
                     value => true}}),

              nei(bind)]};

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(
  internal,
  {setopt,
   #{socket := Socket, level := Level, option := Option, value := Value}},
  _,
  _) ->
    case socket:setopt(Socket, {Level, Option}, Value) of
        ok ->
            keep_state_and_data;

        {error, Reason} ->
            {stop, Reason}
    end;

handle_event(internal, bind = EventName, _, _) ->
    {keep_state_and_data,
     nei({EventName,
          #{family => inet,
            port => resp_config:listener(port),
            addr => any}})};

handle_event(internal,
             {bind = EventName, Addr},
             _,
             #{socket := Listener} = Data) ->
    case socket:bind(Listener, Addr) of
        ok ->
            {next_state,
             ready,
             Data,
             [nei({telemetry, EventName, #{count => 1}, Addr}),
              nei(listen)]};

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
             Data) ->
    ok = telemetry:execute(
           [resp, listener, EventName],
           Measurements,
           maps:merge(
             maps:with([socket], Data),
             Metadata)),
    keep_state_and_data;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, ok}, reaper, Updated} ->
            {keep_state, Data#{requests := Updated}};

        no_request ->
            ?LOG_ERROR(#{msg => Msg, data => Data}),
            keep_state_and_data;

        no_reply ->
            ?LOG_ERROR(#{msg => Msg, data => Data}),
            keep_state_and_data
    end.
