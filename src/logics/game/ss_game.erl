%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2016 16:18
%%%-------------------------------------------------------------------
-module(ss_game).
-author("tihon").

-behaviour(gen_fsm).

-include("ss_game.hrl").
-include("ss_headers.hrl").

%% API
-export([start_link/1, send_ships/2, fire/2, count_statistics/1]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-export([prepare/2, prepare/3]).  %two players should send their ship packages.
-export([play/2, play/3]).  %playing game. Sending fire packages until game end (or disconnect)

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
send_ships(Gid, Ships) ->
  gen_fsm:sync_send_event(Gid, {ships, Ships}). %TODO syn or call by pid

fire(Gid, Fire) ->
  gen_fsm:sync_send_event(Gid, {fire, Fire}). %TODO syn or call by pid

-spec count_statistics(pid()) -> ok.
count_statistics(Game) ->
  gen_fsm:send_event(Game, statistics).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(list()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Args = [GID | _]) -> %TODO gid is not atom. Register me in syn.
  gen_fsm:start_link({global, GID}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #game_state{}} |
  {ok, StateName :: atom(), StateData :: #game_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([GID, UID1, UID2, Rules]) ->
  Pid1 = ss_utils:uid_to_pid(UID1), %TODO use syn instead
  Pid2 = ss_utils:uid_to_pid(UID2),
  monitor(process, Pid1),
  monitor(process, Pid2),
  RulesMap = #{?FIRES_PER_TURN_HEAD := Shots} = ss_rules_logic:decode_rules(Rules),
  {ok, prepare,
    #game_state{game_id = GID, player1 = {Pid1, UID1}, player2 = {Pid2, UID2}, rules = RulesMap, shots_left = Shots}}.

prepare(_Event, State) ->
  {next_state, prepare, State}.

prepare({ships, Ships}, From, State) ->
  {Action, Response, UState} = ss_game_logic:set_fleet(Ships, From, State),
  {reply, Response, Action, UState};
prepare(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, prepare, State}.

play(_Event, State) ->
  {next_state, play, State}.

play({fire, Fire}, From, State) ->
  case ss_game_logic:fire(Fire, From, State) of
    {end_game, Response, UState} ->
      %TODO count statistics, terminate game
      %TODO remove game from database (if was saved)
      {reply, Response, end_game, UState};
    {Action, Response, UState} ->
      {reply, Response, Action, UState}
  end;
play(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, play, State}.

-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #game_state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #game_state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #game_state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #game_state{}}).
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

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

-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #game_state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #game_state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================