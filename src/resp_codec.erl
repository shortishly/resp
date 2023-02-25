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
-on_load(init/0).


init() ->
    {ok, Types} = file:consult(filename:join(resp:priv_dir(), "type.terms")),
    persistent_term:put(
      types(),
      lists:foldl(
        fun
            ({K, V}, #{encode := Encode, decode := Decode} = A) ->
                A#{encode := Encode#{V => K}, decode := Decode#{K => V}}
        end,
        #{encode => #{}, decode => #{}},
       Types)).


type(Codec, K) ->
    case persistent_term:get(types()) of
        #{Codec := #{K := V}} ->
            V;

        _ ->
            error(badarg, [Codec, K])
    end.


types() ->
    [?MODULE, ?FUNCTION_NAME].


decode(<<Type:8, Encoded/bytes>>) ->
    ?FUNCTION_NAME(type(?FUNCTION_NAME, Type), Encoded).


decode(Type, <<"-1\r\n", Remainder/bytes>>) when Type == bulk;
                                                 Type == array ->
    {{Type, null}, Remainder};

decode(Type, <<"\r\n", Remainder/bytes>>) when Type == null ->
    {null, Remainder};

decode(Type, <<"inf\r\n", Remainder/bytes>>) when Type == double ->
    {{double, inf}, Remainder};

decode(Type, <<"-inf\r\n", Remainder/bytes>>) when Type == double ->
    {{double, ninf}, Remainder};

decode(Type, Encoded) ->
    case split(Encoded) of
        [Value, Remainder] when Type == integer; Type == big_number ->
            {{Type, binary_to_integer(Value)}, Remainder};

        [Value, Remainder] when Type == double ->
            try
                {{Type, binary_to_float(Value)}, Remainder}

            catch error:badarg ->
                {{Type, binary_to_integer(Value)}, Remainder}
            end;

        [Value, Remainder] when Type == array;
                                Type == set;
                                Type == bulk;
                                Type == map;
                                Type == attribute;
                                Type == push;
                                Type == verbatim_string;
                                Type == blob_error ->
            ?FUNCTION_NAME(Type, binary_to_integer(Value), Remainder);

        [<<"t">>, Remainder] when Type == boolean ->
            {{Type, true}, Remainder};

        [<<"f">>, Remainder] when Type == boolean ->
            {{Type, false}, Remainder};

        [Value, Remainder] ->
            {{Type, Value}, Remainder};

        _ ->
            partial
    end.


decode(Type, Count, Encoded) when Type == array;
                                  Type == push;
                                  Type == set;
                                  Type == attribute;
                                  Type == map ->
    ?FUNCTION_NAME(Type, Count, Encoded, []);

decode(verbatim_string = Type, Length, Encoded) ->
    case Encoded of
        <<FormatValue:Length/bytes, "\r\n", Remainder/bytes>> ->
            <<Format:3/bytes, ":", Value/bytes>> = FormatValue,
            {{Type, {binary_to_existing_atom(Format), Value}}, Remainder};

        _ ->
            partial
    end;

decode(Type, Length, Encoded) when Type == bulk; Type == blob_error ->
    case Encoded of
        <<Value:Length/bytes, "\r\n", Remainder/bytes>> ->
            {{Type, Value}, Remainder};

        _ ->
            partial
    end.


decode(Type, 0, Remainder, A) when Type == map;
                                   Type == attribute;
                                   Type == set;
                                   Type == push;
                                   Type == array ->
    {{Type, lists:reverse(A)}, Remainder};

decode(Type, N, Encoded, A) when Type == map; Type == attribute ->
    case decode(Encoded) of
        {K, R0} ->
            case decode(R0) of
                {V, R1} ->
                    ?FUNCTION_NAME(Type, N - 1, R1, [{K, V} | A]);

                partial ->
                    partial
            end;

        partial ->
            partial
    end;

decode(Type, N, Encoded, A) when Type == array;
                                 Type == push;
                                 Type == set ->
    case decode(Encoded) of
        {Decoded, Remainder} ->
            ?FUNCTION_NAME(Type, N - 1, Remainder, [Decoded | A]);

        partial ->
            partial
    end.


encode(null = Type) ->
    [type(?FUNCTION_NAME, Type), crlf()];

encode({verbatim_string = Type, {Format, Value}})
  when Format == txt; Format == mkd ->
    [type(?FUNCTION_NAME, Type),
     integer_to_binary(iolist_size(Value) + 4),
     crlf(),
     atom_to_binary(Format),
     ":",
     Value,
     crlf()];

encode({string = Type, Value}) ->
    [type(?FUNCTION_NAME, Type), Value, crlf()];

encode({error = Type, Value}) ->
    [type(?FUNCTION_NAME, Type), Value, crlf()];

encode({boolean = Type, Value}) ->
    [type(?FUNCTION_NAME, Type),
     case Value of
         true -> "t";
         false -> "f"
     end,
     crlf()];

encode({Type, Value}) when Type == integer; Type == big_number ->
    [type(?FUNCTION_NAME, Type), integer_to_binary(Value), crlf()];

encode({double = Type, inf}) ->
    [type(?FUNCTION_NAME, Type), "inf", crlf()];

encode({double = Type, ninf}) ->
    [type(?FUNCTION_NAME, Type), "-inf", crlf()];

encode({double = Type, Value}) when is_float(Value) ->
    [type(?FUNCTION_NAME, Type), float_to_list(Value, [short]), crlf()];

encode({double = Type, Value}) when is_integer(Value) ->
    [type(?FUNCTION_NAME, Type), integer_to_list(Value), crlf()];

encode({bulk = Type, null}) ->
    [type(?FUNCTION_NAME, Type), "-1", crlf()];

encode({Type, Value}) when Type == bulk;
                           Type == blob_error ->
    [type(?FUNCTION_NAME, Type),
     integer_to_binary(iolist_size(Value)),
     crlf(),
     Value,
     crlf()];

encode({array = Type, null}) ->
    [type(?FUNCTION_NAME, Type), "-1", crlf()];

encode({Type, L}) when Type == map; Type == attribute ->
    [type(?FUNCTION_NAME, Type),
     integer_to_list(length(L)),
     crlf(),
     [[encode(K), encode(V)] || {K, V} <- L]];

encode({Type, L}) when Type == array;
                       Type == push;
                       Type == set ->
    [type(?FUNCTION_NAME, Type),
     integer_to_list(length(L)),
     crlf(),
     [encode(Element) || Element <- L]].


split(Encoded) ->
    binary:split(Encoded, crlf()).


crlf() ->
    <<"\r\n">>.
