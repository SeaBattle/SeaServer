-module(ss_super_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	io:format("~w: Super_sup has started (~w)~n", [?MODULE, self()]),
	ListenerSup = ?CHILD(ss_client_sup, supervisor),
	DatabaseSup = ?CHILD(ss_db_sup, supervisor),
	{ok, {{one_for_one, 5, 10}, [ListenerSup, DatabaseSup]}}.

