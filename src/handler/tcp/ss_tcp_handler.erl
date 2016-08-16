%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2016 18:15
%%%-------------------------------------------------------------------
-module(ss_tcp_handler).
-author("tihon").

-behaviour(gen_server).

%% API
-export([start_link/4, init/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, transport}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Socket, Transport, Opts) ->
  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, once}]),
  gen_server:enter_loop(?MODULE, [], #state{socket = Socket, transport = Transport}).


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp, Socket, Data}, State = #state{socket = Socket, transport = Transport}) ->
  Transport:setopts(Socket, [{active, once}]),
  {_Type, _ProtocolVersion, _ApiVersion, _Body} = ss_main_packet:parse_packet(Data),
%%  ss_handler_logic:,
  Transport:send(Socket, Data),
  {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
  {stop, Reason, State};
handle_info(timeout, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
