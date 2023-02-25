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


-module(resp_util).


-export([is_exported/3]).
-export([snake_case/1]).


is_exported(M, F, A) ->
    _ = case erlang:module_loaded(M) of
            false ->
                code:ensure_loaded(M);

            true ->
                ok
        end,
    erlang:function_exported(M, F, A).


snake_case([_ | _] = Labels) ->
    list_to_atom(lists:concat(lists:join("_", Labels))).
