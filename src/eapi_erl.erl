%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% File    : eapi_erl.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Generate Erlang side driver API 
%%% Created : 19 Mar 2010 by Tony Rogvall <tony@rogvall.se>

-module(eapi_erl).

-export([code/1]).

-import(lists, [map/2, reverse/1, foldl/3]).

-include("eapi.hrl").

code(Api) ->
    code(Api#api.items, Api, [], []).

code([{function,ID}|Is], Api, Ds, Fs) ->
    I = dict:fetch(ID, Api#api.functions),
    case I of
	#api_function { name=Name, index=Ix } ->
	    Func = code_function(I,Api),
	    Def  = ["-define(",
		    mk_usymbol(Api#api.erl_symbol_prefix, "CMD_"),
		    string:to_upper(Name),",",
		    integer_to_list(Ix), ").\n"],
	    code(Is, Api, [Def|Ds], [Func|Fs])
    end;
code([{type,ID}|Is], Api, Ds, Fs) ->
   I = dict:fetch(ID, Api#api.types), 
    case I of
	#api_struct { } ->
	    %% generate a record defintions  -record(abc, { a,b,c })
	    %% generate encoder/decoder
	    RecDef = if I#api_struct.intern -> 
			     [];
			true ->
			     struct_def(I, Api)
		     end,
	    RecEnc = struct_encoder(I, Api),
	    RecDec = struct_decoder(I, Api),
	    code(Is, Api, [RecDef|Ds], [RecEnc,RecDec|Fs]);

	#api_enum { name=_Name, extern=true, enums=Enums } ->
	    Ds1 =
		foldl(
		  fun({EName,Def}, Acc) ->
			  [ ["-define(",
			     string:to_upper(atom_to_list(EName)), ",",
			     integer_to_list(Def),").\n"] | Acc]
		  end, Ds, Enums),
	    code(Is, Api, Ds1, Fs);

	#api_enum { name=Name, enums=Enums } ->
	    %% Generate -define(<Name>_<enum>, <number>).
	    Numbers = lists:seq(0, length(Enums)-1),
	    Ds1 =
		foldl(
		  fun({{EName,_Def},N}, Acc) ->
			  [ ["-define(", 
			     string:to_upper(atom_to_list(EName)), ",",
			     integer_to_list(N),").\n"] | Acc]
		  end, Ds, lists:zip(Enums,Numbers)),
	    Map = [Name,"_map(Key) ->\n",
		   "  case(Key) of\n",
		   format_list(
		     map(
		       fun({EName,_Def}) ->
			       ["    ",atom_to_list(EName), " -> ", 
				"?",string:to_upper(atom_to_list(EName))]
		       end, Enums),
		     ";\n"),
		   "\n  end.\n\n"],
	    Keys = [Name,"_keys() -> \n",
		    "  ","[", format_list(
				map(
				  fun({EName,_Def}) ->
					  atom_to_list(EName)
				  end, Enums),
				","),
		    "].\n\n"],
	    code(Is, Api, Ds1, [Keys,Map|Fs])
    end;
code([], _Api, Ds, Fs) ->
    {reverse(Ds), reverse(Fs)}.


struct_def(S, _Api) ->
    ["-record(",S#api_struct.name,",{\n",
     format_list(
       map(
	 fun(F) -> 
		 io_lib:format("  ~p",[F#api_field.name])
	 end,
	 S#api_struct.fields), ",\n"),
     "\n}).\n"].
%%
%% Generate encoder:
%%     e_struct_<Name>(R) ->
%%            encode_field1(R#name.item1),
%%            encode_field2(R#name.item2),
%%            ...
%%            encode_fieldN(R#name.itemN).
%%
struct_encoder(S, Api) when S#api_struct.erl_encode ->
    Fs = map(
	   fun(F) ->
		   FName = atom_to_list(F#api_field.name),
		   Fld = io_lib:format("~p", [F#api_field.name]),
		   elem_encode(F#api_field.type,
			       erl_name(FName),
			       ["R#",S#api_struct.name,".",Fld],Api)
	   end, S#api_struct.fields),
    Pre  = trim_list(map(fun({B,_}) -> B end, Fs)),
    Post = map(fun({_,C}) -> C end, Fs),
    ["e_struct_",S#api_struct.name,"(R)->\n",
     if Pre == [] ->
	     "";
	true ->
	     ["  ",format_list_(Pre, ",\n  "), ","]
     end,
     "  <<", format_list(Post), ">>.\n\n"];
struct_encoder(_S, _Api) ->
    [].

%%
%% Generate decoder:
%%     d_struct_<Name>(Binary) ->
%%            F1 = decode_field1(Binary),
%%            F2 = decode_field1(Binary),
%%            ...
%%            Fn = decode_field1(Binary)
%%
struct_decoder(S, _Api) when S#api_struct.erl_decode ->
    %% FIXME: normally the driver will pass a encoded structure!
    %% But we may want to decode "raw"
    [];
struct_decoder(_S, _Api) ->
    [].


code_function(F, Api) ->
    PortName = if Api#api.erl_prt_name =:= none ->
		       "EApiPort";
		  true ->
		       Api#api.erl_prt_name
	       end,
    ArgNms = map(fun(A) -> erl_name(A#api_arg.name) end, F#api_function.args),
    ArgNames = if Api#api.erl_prt_name =:= none ->
		       ["EApiPort" | ArgNms];
		  true ->
		       ArgNms
	       end,
    {Body,Args} = function_arguments(F#api_function.args,Api),
    Command = (F#api_function.interface == command) orelse
	(Api#api.erl_default_interface == command),
    [mk_symbol(Api#api.erl_function_prefix,F#api_function.name),
     "(", format_list(ArgNames), ") -> \n",
     if Body =:= "" -> "";
	true -> ["  ",format_list(Body,",\n  "),",\n"]
     end,
     if Command ->
	     [" eapi_drv:command(",
	      PortName, ","
	      "?",mk_usymbol(Api#api.erl_symbol_prefix, "CMD_"),
	      string:to_upper(F#api_function.name),
	      ", [", 
	      format_list(
		map(fun(A=["?object_t(("|_]) -> A;
		       (A) -> ["<<",A,">>"]
		    end, Args)),
	      "])"
	     ];
	true ->
	     ["  eapi_drv:control(",
	      PortName, ","
	      "?",mk_usymbol(Api#api.erl_symbol_prefix, "CMD_"),
	      string:to_upper(F#api_function.name),
	      ", <<", format_list(Args), ">>)"]
     end,
     ".\n" ].

     

function_arguments(Arguments,Api) ->
    function_arguments(Arguments,[],[],Api).
    
function_arguments([A|Args],Body,Call,Api) ->
    case elem_encode(A#api_arg.type,erl_name(A#api_arg.name),Api) of
	{"", EncA} ->
	    function_arguments(Args,Body,[EncA|Call],Api);
	{EncB,EncA} ->
	    function_arguments(Args,[EncB|Body],[EncA|Call],Api)
    end;
function_arguments([], Body, Call,_Api) ->
    {reverse(Body), reverse(Call)}.

elem_encode(Type, Var, Api) ->
    elem_encode(Type, Var, Var, Api).

elem_encode(Type, Var, Expr, Api) ->
    case Type of
	uint8_t   -> {"",["?uint8_t((",Expr,"))"]};
	uint16_t  -> {"",["?uint16_t((",Expr,"))"]};
	uint32_t  -> {"",["?uint32_t((",Expr,"))"]};
	uint64_t  -> {"",["?uint64_t((",Expr,"))"]};
	int8_t    -> {"",["?int8_t((",Expr,"))"]};
	int16_t   -> {"",["?int16_t((",Expr,"))"]};
	int32_t   -> {"",["?int32_t((",Expr,"))"]};
	int64_t   -> {"",["?int64_t((",Expr,"))"]};
	float32_t -> {"",["?float32_t((",Expr,"))"]};
	float64_t -> {"",["?float64_t((",Expr,"))"]};
	float_t   -> {"",["?float_t((",Expr,"))"]};
	int_t     -> {"",["?int_t((",Expr,"))"]};
	integer_t -> {"",["?int_t((",Expr,"))"]};
	int       -> {"",["?int_t((",Expr,"))"]};
	uint_t    -> {"",["?uint_t((",Expr,"))"]};
	pointer_t -> {"",["?pointer_t((",Expr,"))"]};
	object_t  -> {"",["?object_t((",Expr,"))"]};
	size_t    -> {"",["?size_t((",Expr,"))"]};
	ssize_t   -> {"",["?ssize_t((",Expr,"))"]};
	boolean_t ->
	    {[Var,"__bool=if (",Expr, ") -> 1; true -> 0 end"],
	     ["?uint8_t(",Var,"__bool)"]};

	binary_t ->
	    {[Var,"__size = byte_size(",Expr,")"],
	     ["?uint32_t(",Var,"__size),",
	      "(",Expr,")/binary"]};

	string_t ->
	    {[Var,"__bin = list_to_binary([",Expr,"]),",
	      Var,"__size = byte_size(",Var,"__bin)"],
	     ["?uint32_t(",Var,"__size),",
	      Var,"__bin/binary"]};

	{const,_CType,_CValue} ->
	    %% Encode CValue as CType
	    {"", ""};

	{list,LType} ->
	    {EDecl,EDecode} = elem_encode(LType, [Var,"_elem"],
					  [Var,"_elem"], Api),
	    {[Var,"__bin = list_to_binary(lists:map(fun(",Var,"_elem) -> ",
	      format_list([EDecl, ["<<",EDecode,">>"]]),
	      " end, ", Expr, ")),",
	      Var,"__len = length(",Expr,")"],
	     ["?uint32_t(",Var,"__len),",
	      Var,"__bin/binary"]};

	{union,_TList} ->
	    %% Fixme union
	    {"",""};

	{array,_AType} ->
	    %% Value is assumed to be a tuple 
	    {"",""};

	{tuple,_Types} ->
	    %% FIXME: inline simple tuple
	    {"",""};


	#api_enum {name=Name,extern=false} ->
	    %% Global enums are index encoded!
	    elem_encode(int_t, Var, [Name,"_map(",Expr,")"], Api);

	#api_enum { type=EType } ->
	    %% FIXME: translate enum atoms to values
	    elem_encode(EType, Var, Expr, Api);

	#api_struct { name=Name } ->
	    %% FIXME: inline simple struct
	    {[Var,"__bin = e_struct_",Name,"(",Expr,"),",
	      Var,"__size = byte_size(",Var,"__bin)"],
	     ["?uint32_t(",Var,"__size),",
	      Var,"__bin/binary"]};

	Name when is_atom(Name) ->
	    elem_encode(dict:fetch(Name,Api#api.types),Var,Expr,Api)
    end.

%% Remove all empty string elements from a list
trim_list([""|As]) -> trim_list(As);
trim_list([A|As]) ->  [A|trim_list(As)];
trim_list([]) -> [].
    
%%
%% Format a list by injecting separators
%%
format_list(List) ->  format_list(List,",").

format_list(List,Sep) ->
    format_list_(trim_list(List), Sep).
    
format_list_([],_Sep) ->    [];
format_list_([A],_Sep) ->   [A];
format_list_([A|As],Sep) -> [A,Sep|format_list_(As,Sep)].

%%
%% Captialize a atom or string
%%
erl_name(Name) when is_atom(Name) -> erl_name(atom_to_list(Name));
erl_name(Name=[C|_]) when C >= $A, C =< $Z -> Name;
erl_name([C|Cs]) when C >= $a, C =< $z -> [(C-$a)+$A|Cs].


%% concat symbols
mk_symbol("", B) -> B;
mk_symbol(A, B) -> A++"_"++B.

mk_usymbol(A, B) -> string:to_upper(mk_symbol(A,B)).

