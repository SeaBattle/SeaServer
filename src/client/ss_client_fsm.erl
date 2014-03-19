%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. мар 2014 22:41
%%%-------------------------------------------------------------------
-module(ss_client_fsm).
-author("tihon").

-behaviour(gen_fsm).

-include("ss_records.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
	authorize/2,
	authorize/3,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(_Socket) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Socket) ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, Socket, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #client_state{}} |
  {ok, StateName :: atom(), StateData :: #client_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Socket) ->
  % Поток ловит ошибки связанных процессов
%% 	erlang:process_flag(trap_exit, true),
	io:format("~w: has started (~w) with ~p~n", [?MODULE, self(), Socket]),
	% Поток отправляет себе сообщение с указанием принять соединение
	gen_fsm:send_all_state_event(self(), accept),
	{ok, authorize, #client_state{socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Авторизовывает
%%
%% @end
%%--------------------------------------------------------------------
-spec(authorize(_Event, _State) ->
	{next_state, NextStateName :: atom(), NextState :: #client_state{}} |
	{next_state, NextStateName :: atom(), NextState :: #client_state{},
		timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #client_state{}}).
authorize(Event, State) ->
	io:format("~w got ~w, ~p~n", [?MODULE, Event, Event]),
	{next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(authorize(Event :: term(), From :: {pid(), term()},
		State :: #client_state{}) ->
	{next_state, NextStateName :: atom(), NextState :: #client_state{}} |
	{next_state, NextStateName :: atom(), NextState :: #client_state{},
		timeout() | hibernate} |
	{reply, Reply, NextStateName :: atom(), NextState :: #client_state{}} |
	{reply, Reply, NextStateName :: atom(), NextState :: #client_state{},
		timeout() | hibernate} |
	{stop, Reason :: normal | term(), NewState :: #client_state{}} |
	{stop, Reason :: normal | term(), Reply :: term(),
		NewState :: #client_state{}}).
authorize(Event, _From, State) ->
	io:format("~w got ~w, ~p~n", [?MODULE, Event, Event]),
	Reply = ok,
	{reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #client_state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #client_state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #client_state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #client_state{}}).
handle_event(accept, StateName, #client_state{socket = ListenSocket}) ->
	% принимаем соединение. Если ok - приняли. Если ошибка, то будет exception
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	% стартуем новый слушающий поток
	ss_client_sup:start_socket(),
	{next_state, StateName, #client_state{socket = AcceptSocket}};
handle_event(_Event, StateName, State) ->
  io:format("~w: Unknown event (~w)~n", [?MODULE, _Event]),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
  io:format("~w unknown event: (~w)~n", [?MODULE, _Info]),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #client_state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #client_state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
