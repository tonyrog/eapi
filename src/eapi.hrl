%%
%%  Driver module API
%%
-ifndef(__EAPI_HRL__).
-define(__EAPI_HRL__, true).

-record(api,
	{
	  name,                    %% string() module name
	  uname,                   %% upper case string of module
	  items = [],              %% list of all selected elements in api file
	  types,                   %% type dictionary
	  functions,               %% function dictionary
	  index = 1,               %% function index counter
	  c_includes = [],         %% include files for C
	  c_function_prefix = "",  %% function name prefix for C
	  c_symbol_prefix = "",
	  erl_includes = [],         %% include files for Erlang
	  erl_function_prefix = "",  %% function name prefix for Erlang
	  erl_symbol_prefix = "",
	  erl_app_name = "",
	  erl_srv_name = "",
	  erl_prt_name = "",
	  erl_reg_name = "",
	  erl_driver_name = "",
	  erl_default_interface = control,
	  error = []
	}).

-record(api_arg,
	{
	  name,          %% atom() !
	  opts = [in],   %% [in,out,inout]
	  type
	}).

-record(api_function,
	{
	  name,            %% string()  function name 
	  index,           %% integer() function index number 
	  type,            %% function return type
	  extern  = false, %% extern => no implementation wrapper
	  async   = false, %% use async dispatch
	  interface,       %% use port_command/port_control interface
	  args             %% function arguments [#api_arg]
	}).

-record(api_field,
	{
	  name,          %% atom() !
	  opts = [],     %% {size,field_name},{length,field_name} ...
	  type
	}).

-record(api_struct,
	{
	  name,                %% string() struct name
	  extern     = false,  %% extern = erlang only record def
	  intern     = false,  %% record defined in some hrl file
	  c_encode   = false,  %% generate a C encoder
	  c_decode   = false,  %% generate a C decoder
	  erl_encode = false,  %% generate an Erlang encoder
	  erl_decode = false,  %% generate an Erlang decoder
	  fields               %% [#api_field{}]
	}).

-record(api_enum,
	{
	  name,           %% string() type name
	  type,           %% encoding type
	  extern = false, %% extern = erlang only defines
	  enums           %% enumeration
	}).

-endif.

