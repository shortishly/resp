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


-module(resp_config).


-export([client/1]).
-export([enabled/1]).
-export([listener/1]).
-export([protocol/1]).
-export([telemetry/1]).
-import(envy, [envy/1]).


enabled(Name) ->
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => false}).

client(port = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => 6379});

client(hostname = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => "localhost"}).


listener(port = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => 6379});

listener(hostname = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => "localhost"});

listener(backlog = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => 5}).


protocol(callback = Name) ->
    envy(#{caller => ?MODULE,
           type => atom,
           names =>[?FUNCTION_NAME, Name]}).


telemetry(Name) when Name == module; Name == function ->
    envy(#{caller => ?MODULE,
           type => atom,
           names =>[?FUNCTION_NAME, Name]});

telemetry(event_names = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => filename:join(resp:priv_dir(), "telemetry.terms")});

telemetry(config = Name) ->
    envy:get_env(resp,
                 resp_util:snake_case([?FUNCTION_NAME, Name]),
                 [app_env, {default, []}]).
