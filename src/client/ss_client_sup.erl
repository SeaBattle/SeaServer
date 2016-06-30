%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. мар 2014 22:53
%%%-------------------------------------------------------------------
-module(ss_client_sup).
-author("tihon").

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
% Старт нового рабочего согласно шаблону
start_socket() ->	supervisor:start_child(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
		MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
		[ChildSpec :: supervisor:child_spec()]
	}} |
	ignore |
	{error, Reason :: term()}).
init([]) ->
	Opts = [{active, true}, {keepalive, true}, {packet, 0}, {reuseaddr, true}, binary],
	case gen_tcp:listen(4232, Opts) of
		{ok, ListenSocket} ->
			io:fwrite("~w:Listening on port ~p~n", [?MODULE, 4232]),
			RestartStrategy = {simple_one_for_one, 10, 60},
			Listener = {ss_client_fsm, {ss_client_fsm, start_link, [ListenSocket]},
				temporary, 2000, worker, [ss_client_fsm]},
			Children = [Listener],
			spawn_link(fun start_listener_pool/0),
			{ok, {RestartStrategy, Children}};
		{error, Reason} ->
			io:format("Can't start server on ~p port!~nReason: ~p", [4232, Reason]),
			{stop, Reason};
		Other ->
			io:format("Can't start server on ~p port!~nReason: ~p", [4232, Other]),
			{stop, Other}
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% Делается для того, чтобы не было задержки при соединениях.
start_listener_pool() ->
	[start_socket() || _ <- lists:seq(1, 10)],
	ok.