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
init(Socket) -> %TODO протестировать ситуацию с подключением и быстрым закрытием сокета. Будет ли при этом упущен tcp_closed?
	% Поток ловит ошибки связанных процессов
%% 	erlang:process_flag(trap_exit, true),
	io:format("~w: has started (~w) with ~p~n", [?MODULE, self(), Socket]),
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
	%TODO try-catch?
	{Type, ProtocolVersion, ApiVersion, Body} = ss_main_packet:parse_packet(Packet),
	%% 	try
%% 	    ss_auth_packet:parse_packet(Packet)
%% 	catch
%% 	    :  ->
%% 	end
	io:format("~w got packet ~w, PV[~w], AV[~w], Body[~p]~n", [?MODULE, Type, ProtocolVersion, ApiVersion, Body]),
	case
	ss_auth_man:make_auth(Type, Body) of
		ok ->
			ss_main_packet:send_packet(auth_resp_packet, Socket, 1);
		Response ->
			ss_main_packet:send_packet(error_packet, Socket, Response)
	end,
	{next_state, connected, State}.

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
handle_event(accept, StateName, #client_state{socket = ListenSocket}) ->  %TODO клиент подключается только 1 раз 0_о
	% принимаем соединение. Если ok - приняли. Если ошибка, то будет exception
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	io:format("~w new connection!~n", [?MODULE]),
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
handle_info(Info = {tcp, _, _}, StateName, State) -> ?MODULE:StateName(Info, State);
handle_info({tcp_closed, _}, _, State = #client_state{socket = Socket}) ->
	io:format("~w Client ~w disconnected~n", [?MODULE, Socket]),
	gen_tcp:close(Socket),  %TODO возможно здесь нужно не просто закрывать сокет, но уведомлять остальных об отключении клиента
	{stop, normal, State};
handle_info({tcp_error, Socket, Message}, _, State) ->
	gen_tcp:close(Socket),
	io:format("tcp_error: ~w~n", [Message]),
	{stop, normal, State};
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
terminate(normal, _, _) ->
	io:format("Normal terminate~n"),
	ok;
terminate(_Reason, _, #client_state{socket = _Sock}) ->
	io:format("Error in ~p[~p]! terminate reason: ~p~n", [?MODULE, self(), _Reason]),
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
