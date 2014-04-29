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
	gen_fsm:start_link(?MODULE, Socket, []).

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
	% Поток отправляет себе сообщение с указанием принять соединение
	gen_fsm:send_all_state_event(self(), accept),
	{ok, authorize, #client_state{socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Авторизовывает пользователя
%%
%% @end
%%--------------------------------------------------------------------
-spec(authorize(_Event, _State) ->
	{next_state, NextStateName :: atom(), NextState :: #client_state{}} |
	{next_state, NextStateName :: atom(), NextState :: #client_state{},
		timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #client_state{}}).
authorize({tcp, _, Packet}, State = #client_state{socket = Socket}) ->
	{Type, ProtocolVersion, ApiVersion, Body} = ss_main_packet:decode_packet(Packet),
	io:format("~w got packet ~w, PV[~w], AV[~w], Body[~p]~n", [?MODULE, Type, ProtocolVersion, ApiVersion, Body]),
	Response = case ss_auth_man:make_auth(Type, Body) of
		           {ok, Player} -> %TODO здесь может возникнуть ошибка и её нужно будет поймать (или не нужно. Отключать игрока при ошибках сервера или перебрасывать на другой поток?)
			           ss_main_packet:encode_packet(player_packet, Player);
		           Error ->
			           io:format("Got error = ~w~n", [Error]),
			           ss_main_packet:encode_packet(error_packet, Error)
	           end,
	gen_tcp:send(Socket, Response),
	{next_state, connected, State}. %TODO connected only on success

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
authorize(Event, _From, State) -> %TODO Не нужно.
	io:format("~w got sync ~w~n", [?MODULE, Event]),
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
	io:format("~w new connection on ~w!~n", [?MODULE, AcceptSocket]),
	% стартуем новый слушающий поток
	ss_client_sup:start_socket(),
	% старт таймера на отключение клиента
	send_timeout_for_state(StateName),
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
%обработка полученных tcp сообщений от клиента -> перенаправляем в соответствующее состояние
handle_info(Info = {tcp, _, _}, StateName, State) -> ?MODULE:StateName(Info, State);
%обработка закрытий tcp соединения -> отключаемся
handle_info({tcp_closed, _}, _, State = #client_state{socket = Socket}) ->
	io:format("~w Client ~w disconnected~n", [?MODULE, Socket]),
	%TODO возможно здесь нужно уведомлять остальных об отключении клиента
	{stop, normal, State};
%обработка ошибок tcp соединения -> отключаемся
handle_info({tcp_error, _, Message}, _, State) ->
	%TODO возможно здесь нужно уведомлять остальных об отключении клиента
	io:format("tcp_error: ~w~n", [Message]),
	{stop, normal, State};
%обработка таймаута для разных состояний -> либо отключаемся, либо обновляем таймаут
handle_info({timeout, StartState}, StateName, State) ->
	if
		StartState == StateName ->
			io:format("~w client disconnected by timeout~n", [?MODULE]),
			{stop, normal, State};
		true ->
			send_timeout_for_state(StateName),
			{next_state, StateName, State}
	end;
handle_info(_Info, StateName, State) ->
	io:format("~w stateName ~w, state ~w unknown event: (~w)~n", [?MODULE, StateName, State, _Info]),
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
terminate(normal, _, #client_state{socket = Socket}) ->
	io:format("Normal terminate~n"),
	gen_tcp:close(Socket),
	ok;
terminate(_Reason, _, #client_state{socket = Socket}) ->
	io:format("Error in ~p[~p]! terminate reason: ~p~n", [?MODULE, self(), _Reason]),
	gen_tcp:close(Socket),
	%TODO возможно здесь стоит закрыть сокет если он ещё не закрыт
	%TODO скорее всего здесь также нужно уведомить других клиентов об ошибке и отключении этого клиента
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
send_timeout_for_state(StateName) when StateName == authorize ->
	erlang:send_after(500, self(), {timeout, StateName}); %пол-секунды на подключение
send_timeout_for_state(StateName) when StateName == connected ->
	erlang:send_after(3000, self(), {timeout, StateName}); %3 секунды на комнаты
send_timeout_for_state(StateName) ->
	io:format("~w warinng, unknown state: ~w~n", [?MODULE, StateName]),
	erlang:send_after(1000, self(), {timeout, StateName}). %секунда на неизвестное состояние