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
%%%-------------------------------------------------------------------
%%% File    : eapi_drv.erl
%%% Description : Erlang Driver interface
%%% Created : 25 Oct 2009 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------

%%% @hidden
%%% @author Tony Rogvall <tony@rogvall.se>

-module(eapi_drv).

-behaviour(gen_server).

%% -define(debug, true).
%% API
-export([start_link/1, start/1, stop/1]).
-export([open/1]).
-export([create/5, async_create/5, release/4, retain/4]).
-export([control/3, command/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(lists, [foreach/2, seq/2]).

-include("../include/cbuf.hrl").

-ifdef(debug).
-define(dbg(F,A), io:format((F)++"\n",(A))).
-define(dbg_hard(F,A), ok).
-define(drv_path, "debug").
-else.
-define(dbg(F,A), ok).
-define(dbg_hard(F,A), ok).
-define(drv_path, "release").
-endif.

-define(ASYNC_TIMEOUT, 10000).  %% 10s - compilation (may be more?)

-record(s, 
	{
	  port,         %% port to driver
	  reg,          %% ets table
	  reg_name,     %% eapi_drv ets register name
	  srv_name,     %% eapi_drv server name
	  driver_name,  %% eapi_drv driver name
	  prt_name      %% eapi_drv port name
	 }).
%%
%% Args =
%%   {srv_name,    atom()}
%%   {prt_name,   atom()}   != srv_name !
%%   {reg_name,    atom()}
%%   {driver_name, string()}
%%   {app, atom()}
%%   debug | {debug,bool()}
%%
start_link(Args) ->
    {srv_name,SrvName} = proplists:lookup(srv_name, Args),
    gen_server:start_link({local, SrvName}, ?MODULE, Args, []).

start(Args) ->
    {srv_name,SrvName} = proplists:lookup(srv_name, Args),
    gen_server:start({local, SrvName}, ?MODULE, Args, []).

stop(Srv) ->
    gen_server:call(Srv, stop).


create(Port,Srv,Code,CodeDestroy,Args) ->
    case control(Port, Code, Args) of
	{ok,Handle} when is_integer(Handle) ->  %% single new object
	    gen_server:cast(Srv, {create,self(),Handle,CodeDestroy}),
	    {ok, Handle};
	{ok,Handles} when is_list(Handles) ->  %% list of new objects
	    foreach(fun(Handle) ->
			    gen_server:cast(Srv,{create,self(),Handle,CodeDestroy})
		    end, Handles),
	    {ok,Handles};
	Error -> Error
    end.

async_create(Port,Srv,Code,CodeDestroy,Args) ->
    case command(Port,Code,Args) of
	{ok,Handle} when is_integer(Handle) ->  %% single new object
	    gen_server:cast(Srv, {create,self(),Handle,CodeDestroy}),
	    {ok, Handle};
	{ok,Handles} when is_list(Handles) ->  %% list of new objects
	    foreach(fun(Handle) ->
			    gen_server:cast(Srv,{create,self(),Handle,CodeDestroy})
		    end, Handles),
	    {ok,Handles};
	Error -> Error
    end.

release(Port, Srv, Code, Handle) ->
    case control(Port, Code, <<?pointer_t(Handle)>>) of
	ok ->
	    gen_server:cast(Srv, {release,self(),Handle}),
	    ok;
	Error ->
	    Error
    end.

retain(Port, Srv, Code, Handle) ->
    case control(Port, Code, <<?pointer_t(Handle)>>) of
	ok ->
	    gen_server:cast(Srv, {retain,self(),Handle}),
	    ok;
	Error ->
	    Error
    end.

control(Port, Code, Args) ->
    Reply = erlang:port_control(Port, Code, Args),
    case cbuf:decode(Reply) of
	{event,Ref} ->
	    wait_reply(Ref);
	Result ->
	    Result
    end.

%% async call to command(v) interface
command(Port, Code, Args) ->
    CmdRef = random:uniform(16#ffffffff),
    Header = <<?uint32_t(Code),?uint32_t(CmdRef)>>,
    ?dbg("command: code=~w, ref=~w\n", [Code, CmdRef]),
    erlang:port_command(Port,[Header,Args]),
    wait_reply(CmdRef).

wait_reply(CmdRef) ->
    receive
	{eapi_reply,CmdRef,Reply} when is_binary(Reply) ->
	    %% This is a reply from a port command
	    %% (passing binaries with zero copy)
	    cbuf:decode(Reply);
	{eapi_reply,CmdRef,Reply} ->
	    %% FIXME: Reply is binary?
	    %% This is a reply from async calls (running in driver thread)
	    Reply
    after ?ASYNC_TIMEOUT ->
	    {error, no_reply}
    end.

%% API to load the driver and start the port
open(Args) ->
    {driver_name,Driver} = proplists:lookup(driver_name, Args),
    {app,App}           = proplists:lookup(app, Args),
    Path =  
	case code:priv_dir(App) of
	    {error, bad_name} -> ".";
	    Dir -> Dir
	end,
    ?dbg("Load driver '~s' from: '~s'\n", [Driver, Path]),
    case erl_ddll:load_driver(Path, Driver) of
	ok ->
	    Port = erlang:open_port({spawn_driver, Driver}, [binary]),
	    case proplists:lookup(prt_name, Args) of
		none ->
		    {ok,Port};
		{prt_name,PrtName} ->
		    register(PrtName, Port),
		    {ok,Port}
	    end;
	Err={error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    Err
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Args) ->
    case open(Args) of
	{ok,Port} ->
	    {driver_name,Driver} = proplists:lookup(driver_name, Args),
	    {prt_name,PrtName} = proplists:lookup(prt_name, Args),
	    {reg_name,RegName} = proplists:lookup(reg_name, Args),
	    {srv_name,SrvName} = proplists:lookup(srv_name, Args),
	    Reg = ets:new(RegName, [named_table, public, set]),
	    {ok, #s { port = Port, 
		      reg  = Reg,
		      driver_name = Driver,
		      reg_name = RegName,
		      srv_name = SrvName,
		      prt_name = PrtName }};
	{error,Error} ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
handle_call(_Request, _From, S) ->
    Reply = ok,
    {reply, Reply, S}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({create,Pid,Handle,ReleaseCode}, S) ->
    Mon = start_monitor(Pid),
    ets:insert_new(S#s.reg, {Handle,ReleaseCode,1}),
    ets:insert_new(S#s.reg, {Mon, {Pid,Handle}}),
    ets:insert_new(S#s.reg, {{Pid,Handle}, Mon, 1}),
    {noreply, S};
handle_cast({retain,Pid,Handle}, S) ->
    try ets:update_counter(S#s.reg,{Pid,Handle},{3,1}) of
	_N ->
	    {noreply,S}
    catch
	error:_ ->
	    Mon = start_monitor(Pid),
	    ets:update_counter(S#s.reg,Handle,{3,1}),
	    ets:insert_new(S#s.reg, {Mon, {Pid,Handle}}),
	    ets:insert_new(S#s.reg, {{Pid,Handle}, Mon, 1}),
	    {noreply,S}
    end;
handle_cast({release,Pid,Handle}, S) ->
    try ets:update_counter(S#s.reg,{Pid,Handle},{3,-1}) of
	0 ->
	    [{_,Mon,_}] = ets:lookup(S#s.reg, {Pid,Handle}),
	    stop_monitor(Mon),
	    ets:delete(S#s.reg, {Pid,Handle}),
	    ets:delete(S#s.reg, Mon),
	    case ets:update_counter(S#s.reg,Handle,{3,-1}) of
		0 -> ets:delete(S#s.reg, Handle);
		_ -> ok
	    end,
	    {noreply,S};
	_N ->
	    {noreply,S}
    catch
	error:_ ->
	    {noreply,S}
    end;
handle_cast(_Msg, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Port,{data,Data}}, S) when Port == S#s.port ->
    ?dbg_hard("Port: data=~p", [Data]),
    case Data of
	<<?OK, ?uint32_t(CmdId), ReplyData/binary>> ->
	    ?dbg("Got: OK cmdid=~w, data=~p", [CmdId,ReplyData]),
	    S1 = case cbuf:decode(ReplyData) of
		     false ->
			 eapi_reply(CmdId, ok, S);
		     {value,Decoded} ->
			 eapi_reply(CmdId, {ok,Decoded}, S)
		 end,
	    {noreply, S1};
	<<?ERROR, ?uint32_t(CmdId), ReplyData/binary>> ->
	    ?dbg("Got: ERROR cmdid=~w, data=~p", [CmdId,ReplyData]),
	    S1 = case cbuf:decode(ReplyData) of
		     false ->
			 eapi_reply(CmdId, error, S);
		     {value,Decoded} ->
			 eapi_reply(CmdId, {error,Decoded}, S)
		 end,			 
	    {noreply, S1};
	<<?EVENT, ?uint32_t(EventId), EventData/binary>> ->
	    ?dbg("Got: EVENT evtid=~w, data=~p", [EventId,EventData]),
	    case ets:lookup(S#s.reg, EventId) of
		[{_,Pid}|_] when is_pid(Pid) ->
		    eapi_event(Pid, EventData, S);
		[_, {_,Pid}|_] when is_pid(Pid) ->
		    eapi_event(Pid, EventData, S);
		_ ->
		    ?dbg("no receipient for evtid=~p data=~p", 
			 [EventId, cbuf:decode(EventData)]),
		    {noreply, S}
	    end;
	_ ->
	    ?dbg("got bad info data ~p\n", [Data]),
	    {noreply, S}
    end;
handle_info({Port,eof}, S) when Port == S#s.port ->
    ?dbg("cl_drv closed",[]),
    erlang:port_close(Port),
    {stop, closed, S};
handle_info({eapi_error,_Ref,Error}, S) ->
    %% This is the context error notification message
    io:format("EDRV Error: ~s\n", [Error]),
    {noreply, S};
handle_info({'DOWN',Mon,process,Pid,_Reason}, S) ->
    case ets:lookup(S#s.reg, Mon) of
	[{_,PH={Pid,Handle}}] ->
	    [{_, ReleaseCode,HCount}] = ets:lookup(S#s.reg, Handle),
	    [{_, Mon,Count}]  = ets:lookup(S#s.reg, PH),
	    foreach(fun(_I) ->
			    control(S#s.port,ReleaseCode,<<?pointer_t(Handle)>>),
			    ?dbg_hard("Handle ~w released\n", [Handle])
		    end, seq(1, Count)),
	    ets:delete(S#s.reg, Mon),
	    ets:delete(S#s.reg, PH),
	    if HCount == 1 ->
		    ets:delete(S#s.reg, Handle);
	       true ->
		    ets:update_counter(S#s.reg,Handle,{3,-1})
	    end;
	[] ->
	    ok
    end,
    {noreply, S};
handle_info(_Info, S) ->
    {noreply, S}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _S) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

eapi_event(Pid, EventData, S) ->	    
    case cbuf:decode(EventData) of
	false ->
	    ?dbg("bad event data ~p", [EventData]),
	    {noreply, S};
	{value, _E={eevent,W,destroyed}} -> 
	    ?dbg("event ~p", [_E]),
	    case ets:lookup(S#s.reg, W) of
		[{_,Mon}|_] when is_reference(Mon) ->
		    ets:delete(S#s.reg, W),
		    ets:delete(S#s.reg, Mon),
		    stop_monitor(Mon),
		    {noreply,S};
		[_,{_,Mon}|_] when is_reference(Mon) ->
		    ets:delete(S#s.reg, W),
		    ets:delete(S#s.reg, Mon),
		    stop_monitor(Mon),
		    {noreply,S};
		[] ->
		    {noreply,S}
	    end;
	{value, E} ->
	    ?dbg("event ~p", [E]),
	    Pid ! E,
	    {noreply, S}
    end.

eapi_reply(_CmdID, _Reply, S) ->
    ?dbg("got reply: ~w  ~p", [_CmdID, _Reply]),
    S.


start_monitor(Pid) ->
    erlang:monitor(process, Pid).

%% stop and flush
stop_monitor(undefined) -> 
    ok;
stop_monitor(Ref) ->
    erlang:demonitor(Ref),
    receive
	{'DOWN',Ref,_,_Pid,_Reason} ->
	    ok
    after 0 ->
	    ok
    end.

