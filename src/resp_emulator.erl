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


-module(resp_emulator).


-export([init/1]).
-export([recv/1]).
-include_lib("kernel/include/logger.hrl").


-record(entry,
        {key :: undefined | binary(),
         hash :: map()}).


init([]) ->
    {ok,
     #{table => ets:new(?MODULE, [{keypos, 2}, public, named_table]),
       protocol => #{version => 2}}}.




recv(#{command := #{name := info}, message := {array, [_]}}) ->
    {continue,
     {encode,
      {bulk,
       ["# Server\r\nredis_version:", resp:version(), "\r\n"]}}};

recv(#{command := #{name := command}, message := {array, _}}) ->
    {continue, {encode, {array, []}}};

recv(#{command := #{name := hello},
       message := {array, [_, {bulk, <<"3">>}]},
       data := #{protocol := Protocol} = Data}) ->
    {continue,
     Data#{protocol := Protocol#{version => 3}},
     {encode,
      {map,
       [{{bulk, <<"server">>}, {bulk, <<"resp">>}},
        {{bulk, <<"version">>}, {bulk, resp:version()}},
        {{bulk,<<"proto">>}, {integer, 3}},
        {{bulk, <<"id">>}, {integer, erlang:phash2(self())}},
        {{bulk, <<"mode">>}, {bulk, <<"standalone">>}},
        {{bulk, <<"role">>}, {bulk, <<"master">>}},
        {{bulk, <<"modules">>}, {array, []}}]}}};

recv(#{command := #{name := hello},
       message := {array, [_, {bulk, <<"2">>}]},
       data := #{protocol := Protocol} = Data}) ->
    {continue,
     Data#{protocol := Protocol#{version => 2}},
     {encode,
      {array,
       [{bulk, <<"server">>}, {bulk, <<"resp">>},
        {bulk, <<"version">>}, {bulk, resp:version()},
        {bulk, <<"proto">>}, {integer, 2},
        {bulk, <<"id">>}, {integer, erlang:phash2(self())},
        {bulk, <<"mode">>}, {bulk, <<"standalone">>},
        {bulk, <<"role">>}, {bulk, <<"master">>},
        {bulk, <<"modules">>}, {array, []}]}}};

recv(#{command := #{name := hset},
       message := {array, [_, {bulk, Key} | NamedValues]}}) ->
    Hash = hash(NamedValues),
    ets:insert(?MODULE, #entry{key = Key, hash = Hash}),
    {continue, {encode, {integer, map_size(Hash)}}};

recv(#{command := #{name := hget},
       message := {array, [_, {bulk, Key}, {bulk, Field}]}}) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            {continue, {encode, {array, []}}};

        [#entry{hash = Hash}] ->
            case maps:find(Field, Hash) of
                {ok, Value} ->
                    {continue, {encode, {bulk, Value}}};

                error ->
                    {continue, {encode, {array, []}}}
            end
    end;

recv(#{command := #{name := hgetall},
       message := {array, [_, {bulk, Key}]}}) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            {continue, {encode, {array, []}}};

        [#entry{hash = Hash}] ->
            {continue,
             {encode,
              {array,
               maps:fold(
                 fun
                     (K, V, A) ->
                         [{bulk, K}, {bulk, V} | A]
                 end,
                 [],
                 Hash)}}}
    end;

recv(#{message := {array, [{bulk, Command} | _]}} = Arg) ->
    ?LOG_DEBUG(#{unknown => Arg}),
    {continue, {encode, {error, ["unknown command '", Command, "'"]}}}.


hash(NamedValues) ->
    ?FUNCTION_NAME(NamedValues, #{}).

hash([{bulk, Key}, {bulk, Value} | T], A) ->
    ?FUNCTION_NAME(T, A#{Key => Value});
hash([], A) ->
    A.
