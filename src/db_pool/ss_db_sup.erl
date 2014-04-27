%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. апр 2014 16:21
%%%-------------------------------------------------------------------
-module(ss_db_sup).
-author("tihon").

-behaviour(supervisor).

%% API
-export([start_link/0, set/2, get/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

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
	{ok, Pools} = seaserver_app:get_conf_param(db_pool, []),
	PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
		PoolArgs = [{name, {local, Name}},
			{worker_module, ss_db_worker}] ++ SizeArgs,
		poolboy:child_spec(Name, PoolArgs, WorkerArgs)
	end, Pools),
	io:format("Creating pool: ~w~n", [PoolSpecs]),
	{ok, {{one_for_one, 10, 10}, PoolSpecs}}.

set(PoolName, Object) ->
	poolboy:transaction(PoolName, fun(Worker) ->
		gen_server:call(Worker, {set, Object})
	end).

get(PoolName, Key, Params) ->
	poolboy:transaction(PoolName, fun(Worker) ->
		gen_server:call(Worker, {get, Key, Params})
	end).


%%%===================================================================
%%% Internal functions
%%%===================================================================
