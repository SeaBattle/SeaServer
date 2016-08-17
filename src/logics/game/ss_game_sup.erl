%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2016 16:18
%%%-------------------------------------------------------------------
-module(ss_game_sup).
-author("tihon").

-behaviour(supervisor).

%% API
-export([start_link/0, start_game/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_game(binary(), binary(), binary(), binary()) -> {ok, pid()}.
start_game(GID, UID1, UID2, Rules) ->
  supervisor:start_child(?MODULE, [GID, UID1, UID2, Rules]).

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
  Game = ?CHILD(ss_game, worker),
  {ok, {{simple_one_for_one, 5, 10}, [Game]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
