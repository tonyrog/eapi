%%
%% Enocde/Decode of the CBUF CTI/ETF format
%%
-module(cbuf).

-export([encode_cti/1, decode_cti/1]).
-export([encode_etf/1, decode_etf/1]).
-export([decode/1]).

-ifdef(debug).
-define(dbg(F,A), io:format((F)++"\n",(A))).
-define(dbg_hard(F,A), ok).
-define(drv_path, "debug").
-else.
-define(dbg(F,A), ok).
-define(dbg_hard(F,A), ok).
-define(drv_path, "release").
-endif.


-include("../include/cbuf.hrl").

encode_etf(Term) ->
    term_to_binary(Term).

encode_cti(Term) ->
    list_to_binary([encode_cti_(Term)]).

encode_cti_(true) ->   
    <<?BOOLEAN, 1>>;
encode_cti_(false) ->  
    <<?BOOLEAN, 0>>;
encode_cti_(X) when is_atom(X) ->
    Val = atom_to_list(X),
    [<<?ATOM, (length(Val)):8>>, Val];
encode_cti_(X) when is_integer(X) ->
    [<<?INT, ?int_t(X)>>];
encode_cti_(X) when is_float(X) -> 
    <<?FLOAT, ?float_t(X)>>;
encode_cti_(X) when is_list(X) ->
    case is_string(X) of
        true ->
	    encode_cti_string(X);
        false ->
	    encode_cti_list(X)
    end;
encode_cti_(X) when is_tuple(X) ->
    encode_cti_tuple(X);
encode_cti_(X) when is_binary(X) ->
    encode_cti_binary(X).

encode_cti_binary(Binary) ->
    Sz = byte_size(Binary),
    [<<?BINARY,?uint32_t(Sz)>>, Binary].

encode_cti_string(String) ->
    Len = length(String),
    if Len =< 255 ->
	    [<<?STRING1, ?uint8_t(Len)>>, String];
       true ->
	    [<<?STRING4, ?uint32_t(Len)>>, String]
    end.
    
encode_cti_list(List) ->
    [?LIST | encode_cti_list_(List)].
    
encode_cti_list_([H|T]) ->
    [encode_cti_(H) | encode_cti_list_(T)];
encode_cti_list_([]) ->
    [?LIST_END].

encode_cti_tuple(Tuple) ->
    [?TUPLE | encode_cti_tuple_(Tuple, size(Tuple), [?TUPLE_END])].

encode_cti_tuple_(_T, 0, Acc) ->
    Acc;
encode_cti_tuple_(T, I, Acc) ->
    encode_cti_tuple_(T, I-1, [encode_cti_(element(I, T)) | Acc]).
     
is_string([X|Xs]) when X >= 0, X =< 255 -> is_string(Xs);
is_string([]) -> true;
is_string(_) -> false.

%% decode reply data
decode(Data = <<131,_/binary>>) ->
    decode_etf(Data);
decode(Data) -> 
    ?dbg_hard("deocde: data = ~p\n", [Data]),
    decode_cti(Data).

decode_etf(Data) ->
    binary_to_term(Data).

decode_cti(Binary) ->
    decode_cti(Binary, []).

decode_cti(<<>>, [Hd]) -> 
    ?dbg_hard("deocde = ~p\n", [Hd]),
    Hd;
decode_cti(Data, Stack) ->
    case Data of
	<<?OK, Rest/binary>> -> 
            ?dbg_hard("OK",[]),
            decode_cti(Rest, [ok|Stack]);
	<<?ERROR, Rest/binary>> -> 
            ?dbg_hard("ERROR",[]),
            decode_cti(Rest, [error|Stack]);
	<<?EVENT, Rest/binary>> -> 
            ?dbg_hard("EVENT",[]),
            decode_cti(Rest, [event|Stack]);
        <<?LIST, Rest/binary>> -> 
            ?dbg_hard("LIST",[]),
            decode_cti(Rest, [list|Stack]);
        <<?TUPLE, Rest/binary>> ->
            ?dbg_hard("TUPLE",[]),
            decode_cti(Rest, [tuple|Stack]);
        <<?BOOLEAN, B, Rest/binary>> -> 
            ?dbg_hard("BOOLEAN:~w",[B]),
            decode_cti(Rest, [B =/= 0 | Stack]);
        <<?UINT8, ?uint8_t(I), Rest/binary>> -> 
            ?dbg_hard("UINT8:~w",[I]),
            decode_cti(Rest, [I|Stack]);
        <<?UINT16, ?uint16_t(I), Rest/binary>> ->
            ?dbg_hard("UINT16:~w",[I]),
            decode_cti(Rest, [I|Stack]);
        <<?UINT32, ?uint32_t(I), Rest/binary>> ->
            ?dbg_hard("UINT32:~w",[I]),
            decode_cti(Rest, [I|Stack]);
        <<?UINT64, ?uint64_t(I), Rest/binary>> ->
            ?dbg_hard("UINT64:~w",[I]),
            decode_cti(Rest, [I|Stack]);
        <<?INT8, ?int8_t(I), Rest/binary>> ->
            ?dbg_hard("INT8:~w",[I]),
            decode_cti(Rest, [I|Stack]);
        <<?INT16, ?int16_t(I), Rest/binary>> ->
            ?dbg_hard("INT16:~w",[I]),
            decode_cti(Rest, [I|Stack]);
        <<?INT32, ?int32_t(I), Rest/binary>> -> 
            ?dbg_hard("INT32:~w",[I]),
            decode_cti(Rest, [I|Stack]);
        <<?INT64, ?int64_t(I), Rest/binary>> ->
            ?dbg_hard("INT64:~w",[I]),
            decode_cti(Rest, [I|Stack]);
        <<?FLOAT32, ?float32_t(F), Rest/binary>> ->
            ?dbg_hard("FLOAT32:~w",[F]),
            decode_cti(Rest, [F|Stack]);
        <<?FLOAT64, ?float64_t(F), Rest/binary>> -> 
            ?dbg_hard("FLOAT64:~w",[F]),
            decode_cti(Rest, [F|Stack]);
        <<?STRING1, ?uint8_t(N), String:N/binary, Rest/binary>> -> 
            ?dbg_hard("STRING1: len=~w, ~w",[N,String]),
            decode_cti(Rest, [binary_to_list(String)  | Stack]);
        <<?STRING4, ?uint32_t(N), String:N/binary, Rest/binary>> ->
            ?dbg_hard("STRING4: len=~w, ~w",[N,String]),
            decode_cti(Rest, [binary_to_list(String)  | Stack]);
        <<?BINARY, ?uint32_t(N), Bin:N/binary, Rest/binary>> -> 
            ?dbg_hard("BINARY: len=~w, ~w",[N,Bin]),
            decode_cti(Rest, [Bin | Stack]);
	<<?ATOM, ?uint8_t(N), Atom:N/binary, Rest/binary>> -> 
            ?dbg_hard("ATOM: len=~w, ~w",[N,Atom]),
            decode_cti(Rest, [list_to_atom(binary_to_list(Atom)) | Stack]);
        <<?LIST_END, Rest/binary>> ->
            ?dbg_hard("LIST_END",[]),
            {L,[_|Stack1]} = lists:splitwith(fun(X) -> X =/= list end, Stack),
            decode_cti(Rest, [lists:reverse(L) | Stack1]);
        <<?TUPLE_END, Rest/binary>> ->
            ?dbg_hard("TUPLE_END",[]),
            {L,[_|Stack1]}=lists:splitwith(fun(X) -> X =/= tuple end, Stack),
            decode_cti(Rest, [list_to_tuple(lists:reverse(L)) | Stack1])
    end.
