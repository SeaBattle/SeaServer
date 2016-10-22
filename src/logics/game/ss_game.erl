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
-export([start_link/1, send_ships/2, fire/2, count_statistics/1, try_join_game/3]).

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
-spec send_ships(pid(), list(map())) -> true | {error, ss_types:code()}.
send_ships(Game, Ships) ->
  gen_fsm:sync_send_event(Game, {ships, Ships}).

fire(Game, Fire) ->
  gen_fsm:sync_send_event(Game, {fire, Fire}).

-spec count_statistics(pid()) -> ok.
count_statistics(Game) ->
  gen_fsm:send_event(Game, statistics).

-spec try_join_game(pid(), ss_types:uid(), pid()) -> boolean().
try_join_game(Game, UID, Pid) ->
  gen_fsm:sync_send_all_state_event(Game, {join, UID, Pid}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(list()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_fsm:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #game_state{}} |
  {ok, StateName :: atom(), StateData :: #game_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([GID, UID1, UID2, Rules]) ->
  Pid1 = syn:find_by_key(UID1),
  Pid2 = syn:find_by_key(UID2),
  _Ref1 = monitor(process, Pid1), %TODO save refs to state?
  _Ref2 = monitor(process, Pid2),
  ok = syn:register(GID, self()),
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

handle_sync_event({join, UID, Pid}, _From, StateName, State = #game_state{player1 = P1, player2 = P2}) ->  %TODO do we need to change state?
  {_, U1} = P1, {_, U2} = P2,
  case UID of %TODO check if all players are online - what should we do?
    UID when UID =:= U1; UID =:= U2 ->
      monitor(process, Pid),  %TODO map instead game_state %TODO save pid to state_map
      {reply, true, StateName, State};
    _ ->
      {reply, false, StateName, State}
  end;
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