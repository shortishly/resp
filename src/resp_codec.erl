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


-module(resp_codec).


-export([decode/1]).
-export([encode/1]).
-include("resp.hrl").


type(?BULK) -> bulk;
type(?ARRAY) -> array;
type(?STRING) -> string;
type(?ERROR) -> error;
type(?INTEGER) -> integer.


decode(<<Type:8, Encoded/bytes>>) ->
    ?FUNCTION_NAME(type(Type), Encoded).


decode(Type, <<"-1\r\n", Remainder/bytes>>) when Type == bulk; Type == array ->
    {{Type, null}, Remainder};

decode(Type, Encoded) ->
    case split(Encoded) of
        [Value, Remainder] when Type == integer ->
            {{Type, binary_to_integer(Value)}, Remainder};

        [Value, Remainder] when Type == array; Type == bulk ->
            ?FUNCTION_NAME(Type, binary_to_integer(Value), Remainder);

        [Value, Remainder] ->
            {{Type, Value}, Remainder};

        _ ->
            partial
    end.


decode(array = Type, Count, Encoded) ->
    ?FUNCTION_NAME(Type, Count, Encoded, []);

decode(bulk = Type, Length, Encoded) ->
    case Encoded of
        <<Value:Length/bytes, "\r\n", Remainder/bytes>> ->
            {{Type, Value}, Remainder};

        _ ->
            partial
    end.


decode(array = Type, 0, Remainder, A) ->
    {{Type, lists:reverse(A)}, Remainder};

decode(array = Type, N, Encoded, A) ->
    case decode(Encoded) of
        {Decoded, Remainder} ->
            ?FUNCTION_NAME(Type, N - 1, Remainder, [Decoded | A]);

        partial ->
            partial
    end.


encode({string, Value}) ->
    [?STRING, Value, ?CRLF];

encode({error, Value}) ->
    [?ERROR, Value, ?CRLF];

encode({integer, Value}) ->
    [?INTEGER, integer_to_binary(Value), ?CRLF];

encode({bulk, null}) ->
    [?BULK, "-1", ?CRLF];

encode({bulk, Value}) ->
    [?BULK, integer_to_binary(iolist_size(Value)), ?CRLF, Value, ?CRLF];

encode({array, null}) ->
    [?ARRAY, "-1", ?CRLF];

encode({array, L}) ->
    [?ARRAY,
     integer_to_list(length(L)),
     ?CRLF,
     [encode(Element) || Element <- L]].


split(Encoded) ->
    binary:split(Encoded, ?CRLF).
