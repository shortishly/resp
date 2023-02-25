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


-module(resp_command).


-export([info/1]).
-on_load(init/0).


info(Command) ->
    case persistent_term:get(?MODULE) of
        #{Command := Detail} ->
            Detail;

        _ ->
            error(badarg, [Command])
    end.


init() ->
    persistent_term:put(
      ?MODULE,
      filelib:fold_files(
        filename:join([resp:priv_dir(), "commands"]),
        ".json$",
        true,
        fun
            (Filename, A) ->
                {ok, JSON} = file:read_file(Filename),
                maps:merge(casefold(jsx:decode(JSON)), A)
        end,
        #{})).


casefold(M) ->
    maps:fold(
      fun
          (Key, Value, A) ->
              A#{string:casefold(Key) => Value}
      end,
      #{},
      M).
