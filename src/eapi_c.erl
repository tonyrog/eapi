%%% File    : eapi_erl.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Generate C side driver API 
%%% Created : 19 Mar 2010 by Tony Rogvall <tony@rogvall.se>

-module(eapi_c).

-export([code/1]).

-include("eapi.hrl").

-import(lists, [map/2, reverse/1, foldl/3, foldr/3]).

code(Api) ->
    code(Api#api.items, Api, [], [], []).

code([{function,ID}|Is], Api, Ds, Fs, Es) ->
    I = dict:fetch(ID, Api#api.functions),
    case I of
	#api_function {name=Name,index=Ix} ->
	    Case = code_dispatch(I,Api),
	    Extern = code_extern(I,Api),
	    Def  = ["#define ",
		    mk_usymbol(Api#api.c_symbol_prefix, "CMD_"),
		    string:to_upper(Name)," ",
		    integer_to_list(Ix), "\n"],
	    code(Is, Api, [Extern,Def|Ds], Fs, [Case|Es])
    end;
code([{type,ID}|Is], Api, Ds, Fs, Es) ->
    I = dict:fetch(ID, Api#api.types),
    case I of
	#api_struct {} ->
	    {E_decl,E_code} = struct_encoder(I, Api),
	    {D_decl,D_code} = struct_decoder(I, Api),
	    {R_decl,R_code} = struct_release(I, Api),
	    Ds1 = if I#api_struct.extern -> Ds;
		     true -> [struct_def(I,Api)|Ds]
		  end,
	    code(Is, Api, [E_decl,D_decl,R_decl|Ds1], 
		 [E_code,D_code,R_code|Fs], Es);
	
	#api_enum{extern=true} ->
	    code(Is, Api, Ds, Fs, Es);

	#api_enum{name=Name,enums=Enums} ->
	    KvDef =
		["eapi_kv_t ", Name,"_kv[] = {\n",
		 format_list(
		   foldl(
		     fun({EName,Def},Acc) ->
			     [ ["  { ", $",atom_to_list(EName), $", ", ",
				if is_atom(Def) -> atom_to_list(Def);
				   is_integer(Def) -> integer_to_list(Def)
				end,
				"}"] | Acc]
		     end, ["  {0, 0}"], Enums),
		   ",\n"),
		 "};\n\n"],
	    code(Is, Api, Ds, [KvDef|Fs], Es)
    end;
code([], _Api, Ds, Fs, Es) ->
    %% produce the eapi_dispatch function
    Dispatch =
	["int eapi_dispatch(eapi_ctx_t* ctx, unsigned int cmd, cbuf_t* c_in, cbuf_t* c_out)\n",
	 "{\n",
	 "  switch(cmd) {\n",
	 reverse(Es),
	 "  default:\n",
	 "    return -1;\n",
	 "  }\n",
	 "  return 0;\n",
	 "}\n"],
    {reverse(Ds), reverse([Dispatch|Fs])}.

%%
%% Generate function argument decoder for dispatch function
%%
code_dispatch(I,Api) ->
    {ArgDecl,ArgDecode,ArgPost,ArgList} = 
	dispatch_arguments(I#api_function.args,Api),
    ["  case ", mk_usymbol(Api#api.c_symbol_prefix, "CMD_"), 
     string:to_upper(I#api_function.name), ": {\n", 
     format_seq(ArgDecl, ";\n"),
     format_seq(ArgDecode, ";\n"),
     "    ", mk_symbol(Api#api.c_function_prefix, I#api_function.name),
     "(", format_list(["ctx","c_out"|ArgList], ","), ");\n",
     format_seq(ArgPost, ";\n"),
     "    break;\n",
     "  }\n"].

%% Generate an extern declaration for implementation function
code_extern(I,Api) when I#api_function.extern == false ->
    ArgDecls = extern_arguments(I#api_function.args, Api),
    ["extern void ", mk_symbol(Api#api.c_function_prefix, I#api_function.name),
     "(", format_list(["eapi_ctx_t* ctx", "cbuf_t* c_out" | ArgDecls],","), ")",
     ";\n"];
code_extern(_I,_Api) ->
    %% the function has extern implementaion
    [].

extern_arguments([A|As], Api) ->
    Arg = atom_to_list(A#api_arg.name),
    ADecl =
	case type_to_string(A#api_arg.type,Api,"*") of
	    {TypeName,_Init} ->
		[TypeName," ",Arg];
	    TypeName ->
		[TypeName," ",Arg]
	end,
    [ADecl | extern_arguments(As, Api)];
extern_arguments([], _Api) ->
    [].
    


dispatch_arguments(Args,Api) ->
    dispatch_arguments(Args,Api,[],[],[],[]).

dispatch_arguments([A|As],Api,Decl,Decode,Post,Args) ->
    Arg = atom_to_list(A#api_arg.name),
    ADecl =
	case type_to_string(A#api_arg.type,Api,"") of
	    {TypeName,Init} ->
		["    ",TypeName," ",Arg," = ", io_lib:format("~p", [Init])];
	    TypeName ->
		["    ",TypeName," ",Arg]
	end,
    Addr = case eapi:is_pointer_type(A#api_arg.type,Api) of
	       {true,pointer_t} ->
		   ""; %% pass pointer directly
	       {true,_} -> 
		   "&";
	       {false,_} -> 
		   ""
	   end,
    ADec = ["    ",elem_decode(A#api_arg.type, "c_in", ["&",Arg], Api)],
    Post1 = case elem_destroy(A#api_arg.type, [Arg], Api) of
		[] -> Post;
		Destroy -> [["    ",Destroy]|Post]
	    end,
    dispatch_arguments(As,Api,[ADecl|Decl],[ADec|Decode],Post1,
		       [[Addr,Arg]|Args]);
dispatch_arguments([],_Api,Decl,Decode,Post,Args) ->
    {reverse(Decl),reverse(Decode),reverse(Post),reverse(Args)}.

type_to_string(Type,Api,StructPtr) ->
    case Type of
	uint8_t   -> "uint8_t";
	uint16_t  -> "uint16_t";
	uint32_t  -> "uint32_t";
	uint64_t  -> "uint64_t";
	int8_t    -> "int8_t";
	int16_t   -> "int16_t";
	int32_t   -> "int32_t";
	int64_t   -> "int64_t";
	float32_t -> "float32_t";
	float64_t -> "float64_t";
	float_t   -> "float_t";
	int_t     -> "int";
	int       -> "int";
	uint_t    -> "unsigned int";
	pointer_t -> "void*";
	size_t    -> "size_t";
	ssize_t   -> "ssize_t";
	boolean_t -> "uint8_t";
	binary_t  -> ["eapi_binary_t",StructPtr];
	object_t  -> "void*";
	handle_t  -> "void*";
	string_t  -> ["eapi_string_t",StructPtr];
	atom_t    -> "char*";  %% FIXME!!
	{tuple,_Es} -> "void*"; %% FIXME
	{list,EType} -> 
	    [type_to_string(EType,Api,""),"*"];
	{const,CType,CValue} ->
	    {type_to_string(CType,Api,StructPtr), CValue};
	{array,AType} ->
	    [type_to_string(AType,Api,""),"*"];
	#api_enum{name=Name} -> 
	    Name;
	#api_struct{} ->
	    Name = struct_name(Type),
	    ["struct ",Name,StructPtr];
	ID when is_atom(ID) ->
	    case dict:find(ID, Api#api.types) of
		{ok,I=#api_struct{}} -> 
		    ["struct ",struct_name(I),StructPtr];
		_ ->
		    case eapi:is_pointer_type(ID,Api) of
			{true,_} -> [atom_to_list(ID),"*"];
			{false,_} -> atom_to_list(ID)
		    end
	    end
    end.

elem_decode(Type, Src, Dst, Api) ->
    case Type of
	uint8_t   ->
	    ["cbuf_get_uint8(",Src,",",Dst,")"];
	uint16_t  ->
	    ["cbuf_get_uint16(",Src,",",Dst,")"];
	uint32_t  ->
	    ["cbuf_get_uint32(",Src,",",Dst,")"];
	uint64_t  ->
	    ["cbuf_get_uint64(",Src,",",Dst,")"];
	int8_t    ->
	    ["cbuf_get_int8(",Src,",",Dst,")"];
	int16_t   ->
	    ["cbuf_get_int16(",Src,",",Dst,")"];
	int32_t   ->
	    ["cbuf_get_int32(",Src,",",Dst,")"];
	int64_t   ->
	    ["cbuf_get_int64(",Src,",",Dst,")"];
	float32_t ->
	    ["cbuf_get_float32(",Src,",",Dst,")"];
	float64_t ->
	    ["cbuf_get_float64(",Src,",",Dst,")"];
	float_t   ->
	    ["cbuf_get_float(",Src,",",Dst,")"];
	int_t     ->
	    ["cbuf_get_int(",Src,",",Dst,")"];
	int       ->
	    ["cbuf_get_int(",Src,",",Dst,")"];
	uint_t    ->
	    ["cbuf_get_uint(",Src,",",Dst,")"];
	pointer_t ->
	    ["eapi_get_pointer(ctx,",Src,", (void**)",Dst,")"];
	size_t    ->
	    ["cbuf_get_size(",Src,",",Dst,")"];
	ssize_t   ->
	    ["cbuf_get_ssize(",Src,",",Dst,")"];
	boolean_t ->
	    ["cbuf_get_boolean(",Src,",",Dst,")"];
	binary_t  ->
	    ["cbuf_get_nbinary(",Src,",",Dst,")"];
	object_t  ->
	    ["cbuf_get_object(",Src,",",Dst,")"];
	handle_t  ->
	    ["cbuf_get_handle(",Src,",",Dst,")"];
	string_t  -> 
	    ["cbuf_get_nstring(",Src,",",Dst,")"];
	{list, _EType} ->
	    ["GET_LIST(",Src,",",Dst,")"];
	{tuple,_Es} ->
	    ["GET_TUPLE(",Src,",",Dst,")"];
	{const,_CType,_CValue} ->
	    ["GET_CONST(",Src,",",Dst,")"];
	{array,_AType} -> 
	    ["GET_ARRAY(",Src,",",Dst,")"];

	#api_enum{name=Name,type=EType} ->
	    ["{ ", atom_to_list(EType), " kix;\n",
	     "  ", elem_decode(EType, Src, "&kix", Api), ";",
	     "  *(", Dst, ") = ", Name,"_kv[kix].value;\n", 
	     "}"];
	#api_struct{} ->
	    ["d_struct_", struct_name(Type), "(ctx,",Src,",",Dst,")"];
	TName when is_atom(TName) ->
	    elem_decode(dict:fetch(TName, Api#api.types), Src, Dst, Api)
    end.

elem_destroy(Type, Dst, Api) ->
    case Type of
	binary_t  ->
	    ["cbuf_free_binary(&(",Dst,"))"];
	string_t ->
	    ["cbuf_free_string(&(",Dst,"))"];
	#api_struct{} ->
	    ["r_struct_", struct_name(Type), "(ctx,&(",Dst,"))"];
	TName when is_atom(TName) ->
	    case dict:find(TName, Api#api.types) of
		{ok,Type1} ->
		    elem_destroy(Type1, Dst, Api);
		_ -> []
	    end;
	_ -> 
	    []
    end.

elem_encode(Type, Buf, Src0, Size, Api) ->
    Src = encode_src(Src0, Type),
    case Type of
	uint8_t   ->
	    ["cbuf_put_uint8(",Buf,",",Src,")"];
	uint16_t  ->
	    ["cbuf_put_uint16(",Buf,",",Src,")"];
	uint32_t  ->
	    ["cbuf_put_uint32(",Buf,",",Src,")"];
	uint64_t  ->
	    ["cbuf_put_uint64(",Buf,",",Src,")"];
	int8_t    ->
	    ["cbuf_put_int8(",Buf,",",Src,")"];
	int16_t   ->
	    ["cbuf_put_int16(",Buf,",",Src,")"];
	int32_t   ->
	    ["cbuf_put_int32(",Buf,",",Src,")"];
	int64_t   ->
	    ["cbuf_put_int64(",Buf,",",Src,")"];
	float32_t ->
	    ["cbuf_put_float32(",Buf,",",Src,")"];
	float64_t ->
	    ["cbuf_put_float64(",Buf,",",Src,")"];
	float_t   ->
	    ["cbuf_put_float(",Buf,",",Src,")"];
	int_t     ->
	    ["cbuf_put_int(",Buf,",",Src,")"];
	int       ->
	    ["cbuf_put_int(",Buf,",",Src,")"];
	uint_t    ->
	    ["cbuf_put_uint(",Buf,",",Src,")"];
	pointer_t ->
	    ["eapi_put_pointer(ctx,",Buf,",",Src,")"];
	size_t    ->
	    ["cbuf_put_size(",Buf,",",Src,")"];
	ssize_t   ->
	    ["cbuf_put_ssize(",Buf,",",Src,")"];
	boolean_t ->
	    ["cbuf_put_boolean(",Buf,",",Src,")"];
	binary_t  ->
	    ["cbuf_put_binary(",Buf,",",Src,",",Size,")"];
	object_t  ->
	    ["cbuf_put_object(",Buf,",",Src,")"];
	handle_t  ->
	    ["cbuf_put_handle(",Buf,",",Src,")"];
	string_t  ->
	    ["cbuf_put_string(",Buf,",",Src,")"];
	atom_t ->
	    ["cbuf_put_atom(",Buf,",",Src,")"];
	{list, T} ->
	    [["cbuf_put_list_begin(",Buf,",",Size,");\n"],
	     ["  { \n",
	      "     int _lst_i;\n",
	      "     for (_lst_i = 0; _lst_i < ", Size, "; _lst_i++) {\n",
	      "      ", elem_encode(T, Buf, ["&",Src,"[_lst_i]"], "", Api), ";\n",
	      "    }\n",
	      "  }\n"
	     ],
	     ["cbuf_put_list_end(",Buf,",",Size,");\n"]
	    ];
	{array,T} -> 
	    [["cbuf_put_tuple_begin(",Buf,",",Size,");\n"],
	     ["  { \n",
	      "     int _arr_i;\n",
	      "     for (_arr_i = 0; _arr_i < ", Size, "; _arr_i++) {\n",
	      "      ", elem_encode(T, Buf, ["&",Src,"[_arr_i]"], "", Api), ";\n",
	      "    }\n",
	      "  }\n"
	     ],
	     ["cbuf_put_tuple_end(",Buf,",",Size,");\n"]
	    ];
	{tuple,Ts} ->
	    TsSize = integer_to_list(length(Ts)),
	    [["cbuf_put_tuple_begin(",Buf,",",TsSize,");\n"],
	     ["{ void* _tup_ptr = (void*)", Src, ";\n",
	      map(
		fun(T) ->
			case type_to_string(T,Api,"") of
			    {_TStr,Value} ->
				["    ", elem_encode(T, Buf, Value, "", Api),
				 ";\n"];
			    TStr ->
				[elem_encode(T, Buf, "_tup_ptr", "", Api),";\n",
				 "    ", "_tup_ptr += sizeof(",TStr,");\n"]
			end
		end, Ts),
	      "  }\n"],
	     ["cbuf_put_tuple_end(",Buf,",",TsSize,");\n"]
	    ];

	{const,CType,CValue} ->
	    elem_encode(CType,Buf,CValue,Size,Api);

	#api_enum { name=Name, type=EType } ->
	    ["{ ", type_to_string(EType,Api,""), " kix;\n",
	     "  ", elem_encode(EType, Buf, "&kix", "", Api), ";",
	     "  *(", Src, ") = ", Name,"_kv[kix].value;\n", 
	     "}"];

	#api_struct {} ->
	    ["e_struct_", struct_name(Type), "(ctx,",Buf,",",Src,")"];
	TName when is_atom(TName) ->
	    elem_encode(dict:fetch(TName, Api#api.types), Buf, Src, "", Api)
    end.

encode_src(X, _) when is_integer(X) ->
    integer_to_list(X);
encode_src(X, atom_t) when is_atom(X) ->
    quote(atom_to_list(X));
encode_src(X, string_t) when is_atom(X) ->
    quote(atom_to_list(X));
encode_src(X, _) ->
    X.



struct_def(I,Api) ->
    Name = struct_name(I),
    ["struct ", Name," {\n",
     format_seq(
       map(
	 fun(F) -> 
		 io_lib:format("  ~s ~s",
			       [type_to_string(F#api_field.type,Api,""),
				F#api_field.name])
	 end,
	 I#api_struct.fields), ";\n"),
     "\n};\n"].

struct_encoder(I, Api) when I#api_struct.c_encode ->
    Name = struct_name(I),
    N = length(I#api_struct.fields)+1,
    FDecl = ["int e_struct_", Name,
	     "(eapi_ctx_t* ctx, cbuf_t* c_out, ", "struct ", Name, " *ptr)"],
    Code = 
	[FDecl, "\n",
	 "{\n",
	 "  ", "cbuf_put_tuple_begin(c_out,", integer_to_list(N), ");\n",
	 "  ", "cbuf_put_atom(c_out,", $",Name, $", ");\n",
	 format_seq(
	   map(
	     fun(F) ->
		     Src = ["ptr->",atom_to_list(F#api_field.name)],
		     Size = case proplists:lookup(size, F#api_field.opts) of
				none -> "";
				{size,Field} when is_atom(Field) ->
				    ["ptr->", atom_to_list(Field)];
				{size,Sz} when is_integer(Sz) ->
				    [integer_to_list(Sz)]
			    end,
		     ["  ",elem_encode(F#api_field.type,"c_out", Src, Size, Api)]
	     end, I#api_struct.fields), ";\n"),
	 "  ", "cbuf_put_tuple_end(c_out,", integer_to_list(N), ");\n",
	 "  return 0;\n",
	 "}\n\n"],
    Def = ["extern ", FDecl, ";\n"],
    {Def,Code};
struct_encoder(_I, _Api) ->
    {[],[]}.

%% Generate decoder of a struct
struct_decoder(I, Api) when I#api_struct.c_decode ->
    Name = struct_name(I),
    FDecl = ["int d_struct_", Name,
	     "(eapi_ctx_t* ctx, cbuf_t* c_in, ", "struct ", Name, " *ptr)"],
    Code = 
	[FDecl,"\n",
	 "{\n",
	 "  uint32_t d_size;\n",
	 "  cbuf_get_uint32(c_in,&d_size);\n",
	 format_seq(
	   map(
	     fun(F) ->
		     Dst = ["&ptr->",atom_to_list(F#api_field.name)],
		     ["  ",elem_decode(F#api_field.type,"c_in",Dst, Api)]
	     end, I#api_struct.fields), ";\n"),
	 "  return 0;\n",
	 "}\n\n"],
    Def = ["extern ", FDecl, ";\n"],
    {Def, Code};
struct_decoder(_I, _Api) ->
    {[],[]}.

%% Generate release of struct elements
struct_release(I, Api) when I#api_struct.c_decode ->
    Name = struct_name(I),
    FDecl = ["void r_struct_", Name,
	     "(eapi_ctx_t* ctx, ", "struct ", Name, " *ptr)"],
    Seq = foldr(
	    fun(F,Acc) ->
		    Dst = ["ptr->",atom_to_list(F#api_field.name)],
		    case elem_destroy(F#api_field.type,Dst,Api) of
			[] -> Acc;
			Destroy -> [["  ",Destroy] | Acc]
		    end
	    end, [], I#api_struct.fields),
    Code =     
	[FDecl,"\n",
	 "{\n",
	 if Seq =:= 0 ->
		 ["  (void) ctx;\n",
		  "  (void) ptr;\n" ];
	    true ->
		 format_seq(Seq, ";\n")
	 end,
	 "}\n\n"],
    Def = ["extern ", FDecl, ";\n"],
    {Def, Code};
struct_release(_I, _Api) ->
    {[],[]}.

%%
%% Format a list by injecting separators
%%
    
format_list([],_Sep) ->    [""];
format_list(["",""|As],Sep) -> format_list([""|As],Sep);
format_list(["",A],_Sep) -> [A];
format_list([A],_Sep) ->   [A];
format_list(["",A|As],Sep) -> [A,Sep | format_list(As,Sep)];
format_list([A|As],Sep) -> [A,Sep | format_list(As,Sep)].

%%
%%
%%
    
format_seq([""|As],Sep) -> format_seq(As, Sep);
format_seq([A|As],Sep) -> [A,Sep | format_seq(As,Sep)];
format_seq([],_Sep) -> [].

%% concat symbols
mk_symbol("", B) -> B;
mk_symbol(A, B) -> A++"_"++B.

mk_usymbol(A, B) -> string:to_upper(mk_symbol(A,B)).

quote(String) ->
    [$", String, $"].

struct_name(I) ->
    if I#api_struct.intern ->
	    "i_"++ I#api_struct.name;
       true ->
	    I#api_struct.name
    end.
