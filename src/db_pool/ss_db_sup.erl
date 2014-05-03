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
-export([start_link/0, put/2, get/3, put_async/2, put/3]).

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
	Pools = seaserver_app:get_conf_param(pools, []),
	PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
		PoolArgs = [{name, {local, Name}},
			{worker_module, ss_db_worker}] ++ SizeArgs,
		poolboy:child_spec(Name, PoolArgs, WorkerArgs)
	end, Pools),
	{ok, {{one_for_one, 10, 10}, PoolSpecs}}.

put(PoolName, Object) ->
	poolboy:transaction(PoolName, fun(Worker) ->
		gen_server:call(Worker, {put, Object})
	end).
put(PoolName, Object, Params) ->
	poolboy:transaction(PoolName, fun(Worker) ->
		gen_server:call(Worker, {put, Object, Params})
	end).

put_async(PoolName, Object) ->
	poolboy:transaction(PoolName, fun(Worker) ->
		gen_server:cast(Worker, {put, Object})
	end).

get(PoolName, Bucket, Key) ->
	poolboy:transaction(PoolName, fun(Worker) ->
		gen_server:call(Worker, {get, {Bucket, Key}})
	end).


%%%===================================================================
%%% Internal functions
%%%===================================================================
