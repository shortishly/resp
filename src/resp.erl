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


-module(resp).


-export([priv_dir/0]).
-export([start/0]).
-export([version/0]).
-export_type([int64/0]).
-export_type([resp2/0]).
-export_type([resp3/0]).
-export_type([tv/0]).

-type int64() :: -9_223_372_036_854_775_808..9_223_372_036_854_775_807.

-type resp2() :: {array, [tv()] | null}
               | {bulk, iodata() | null}
               | {string, iodata()}
               | {error, iodata()}
               | {integer, int64()}.

-type resp3() :: null
               | {double, float()}
               | {boolean, boolean()}
               | {verbatim_string, iodata()}
               | {map, [tv()]}
               | {set, [tv()]}
               | {attribute, [tv()]}
               | {push, iodata()}
               | {big_number, integer()}.

-type tv() :: resp2()
            | resp3().

version() ->
    {ok, Version} = application:get_key(resp, vsn),
    Version.


start() ->
    application:ensure_all_started(?MODULE).


priv_dir() ->
    code:priv_dir(?MODULE).
