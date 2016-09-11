%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Sep 2016 22:27
%%%-------------------------------------------------------------------
-module(ss_game_logic).
-author("tihon").

-include("ss_headers.hrl").
-include("ss_game.hrl").
-include("ss_codes.hrl").

-define(PROPER(A, B), A >= 0, A =< 9, B >= 0, B =< 9).
-define(NORMAL(X), X =:= ?NORMAL_TYPE; X =:= ?MULTI_TYPE).

%% API
-export([set_fleet/3, fire/3]).

%% Generate map and set fleet. If last player sets his fleet - game starts.
%% First, who sets the fleet - will be first to fire
-spec set_fleet(list(map()), pid(), #game_state{}) -> {atom(), true | tuple(), #game_state{}}.
set_fleet(Ships, Pid,
    State = #game_state{player1 = {P1, _}, player2 = {P2, _}, rules = Rules}) when Pid =:= P1; Pid =:= P2 ->
  #game_state{fleet_player1 = F1, fleet_player2 = F2} = State,
  Formed = form_ships(Ships),
  try ss_map_logic:place_ships(Ships, Rules) of
    Map when Pid =:= P1, F2 =:= undefined ->  %fleet belongs to player1, player2 fleet is empty
      {prepare, true, State#game_state{fleet_player1 = Map, ships_player1 = Formed}};
    Map when Pid =:= P2, F1 =:= undefined ->  %fleet belongs to player2, player1 fleet is empty
      {prepare, true, State#game_state{fleet_player2 = Map, ships_player2 = Formed}};
    Map when Pid =:= P1 ->  %fleet belongs to player1, player2 fleet was already set. Player2 will be first to fire. Start the game
      UState = State#game_state{fleet_player1 = Map, active = 1, ships_player1 = Formed},
      start_game(UState),
      {play, true, UState};
    Map when Pid =:= P2 ->  %fleet belongs to player2, player1 fleet was already set. Player1 will be first to fire. Start the game
      UState = State#game_state{fleet_player2 = Map, active = 0, ships_player2 = Formed},
      start_game(UState),
      {play, true, UState}
  catch
    throw:{error, Code} ->
      {prepare, {error, Code}, State}
  end;
set_fleet(_, _, State) -> %fleet package was got from unknown player. May be wrong game.
  {prepare, {error, ?WRONG_GAME}, State}.

%% Fire (or another action, which takes player's turn). Update map, change turn if needed,
%% update players ships. If no ships left - end game.
fire(Shot = #{?FIRE_TYPE_HEAD := X, ?SHOT_HEAD := Shots}, Pid, State = #game_state{shots_left = M}) when ?NORMAL(X), length(Shots) =:= M ->
  case State of
    #game_state{player1 = {Pid, _}, player2 = {Pid2, _}, active = 0} -> %turn of player1 and he makes it
      #game_state{fleet_player2 = Map2, ships_player2 = Ships2} = State,
      do_fire(Shot, Shots, Map2, Ships2, Pid2, State);
    #game_state{player2 = {Pid, _}, player1 = {Pid1, _}, active = 1} -> %turn of player2 and he makes it
      #game_state{fleet_player1 = Map1, ships_player1 = Ships1} = State,
      do_fire(Shot, Shots, Map1, Ships1, Pid1, State);
    _ ->
      {play, {error, ?WRONG_TURN}, State}
  end;
fire(Fire, Pid, State) ->
  case maps:is_key(?FIRE_TYPE_HEAD, Fire) of
    false -> fire(Fire#{?FIRE_TYPE_HEAD => ?NORMAL_TYPE}, Pid, State);  %default fire type is normal
    true -> {play, {error, ?BAD_PACKAGE}, State}
  end.

%% @private    %TODO save shots_left from hits
do_fire(Shot, Shots, Map, Ships, Pid, State) ->
  notify_player(Pid, {fire, Shot}),
  case do_fire_cycle(Shots, Map, Ships, State) of %TODO return coordinates of hit (and hit number)
    A = {end_game, _} -> A;
    {0, UState} -> {play, false, UState};
    {_, UState} -> {play, true, UState}
  end.

%% @private
do_fire_cycle(Shots, Map, Ships, State) ->
  lists:foldl(
    fun
      (_, {end_game, UState}) -> {end_game, UState};
      (#{?FIRE_X_HEAD := X, ?FIRE_Y_HEAD := Y}, {Hits, UState = #game_state{shots_left = M}}) ->
        do_one_shot(Map, X, Y, Ships, UState#game_state{shots_left = M - 1}, Hits)
    end, {0, State}, Shots).

%% @private
do_one_shot(Map, X, Y, Ships, State = #game_state{player1 = {P1, _}, player2 = {P2, _}, active = N, rules = Rules}, Hits) ->
  case mark_fire(Map, X, Y, Ships) of
    {true, [], UMap2} ->   %ship was hit (no more ships for player)
      notify_active_player(P1, P2, N, win),
      notify_active_player(P1, P2, 1 - N, loose),
      {end_game, State#game_state{fleet_player2 = UMap2, ships_player2 = []}};
    {true, UShips2, UMap2} ->   %ship was hit
      N = get_repeat_shot(Rules),
      UState = should_change_turn(State, Hits + N),
      {Hits + N, UState#game_state{fleet_player2 = UMap2, ships_player2 = UShips2}};
    {false, UMap2} ->  %miss, change turn
      UState = should_change_turn(State, Hits),
      {Hits, UState#game_state{fleet_player2 = UMap2}}
  end.

%% @private
get_repeat_shot(#{?REPEAT_ON_HIT_HEAD := true}) -> 1;
get_repeat_shot(#{?REPEAT_ON_HIT_HEAD := false}) -> 0.

%% @private
-spec should_change_turn(#game_state{}, integer()) -> {integer(), #game_state{}}.
should_change_turn(State = #game_state{shots_left = 0, rules = #{?FIRES_PER_TURN_HEAD := M}}, 0) ->   %no hits, shots ended. Change turn
  #game_state{player1 = {P1, _}, player2 = {P2, _}, active = N} = State,
  notify_turn(P1, P2, 1 - N),
  State#game_state{active = 1 - N, shots_left = M};
should_change_turn(State = #game_state{rules = Rules, shots_left = 0}, Hits) when Hits > 0 ->   %was some hits, shots ended. Should change turn depends on rules
  #game_state{player1 = {P1, _}, player2 = {P2, _}, active = N} = State,
  case Rules of
    #{?REPEAT_ON_HIT_HEAD := true} -> %repeat turn on hit
      notify_turn(P1, P2, N),
      State;
    #{?REPEAT_ON_HIT_HEAD := false, ?FIRES_PER_TURN_HEAD := M} ->  %no repeat on hit - change turn
      notify_turn(P1, P2, 1 - N),
      State#game_state{active = 1 - N, shots_left = M}
  end;
should_change_turn(State, _) ->   %hit or miss - do not change the turn, other shots left (case of multishot)
  State.

%% @private
mark_fire(Map, X, Y, Ships) ->
  case ss_map_logic:perform_fire(Map, X, Y) of
    {true, Id, UMap2} ->
      {true, fire_at_ship(Ships, Id), UMap2};
    {false, UMap2} ->
      {false, Ships, UMap2}
  end.

%% @private
fire_at_ship(Ships, N) ->
  case lists:keyfind(N, 1, Ships) of
    {N, 1} -> lists:keydelete(N, 1, Ships);
    {N, Size} -> lists:keyreplace(N, 1, Ships, {N, Size - 1})
  end.

%% @private
start_game(#game_state{player1 = {P1, _}, player2 = {P2, _}, active = ActiveNum}) ->
  notify_player(P1, start),
  notify_player(P2, start),
  notify_active_player(P1, P2, ActiveNum, turn),
  notify_active_player(P1, P2, 1 - ActiveNum, enemy_turn).

%% @private
form_ships(Ships) ->
  lists:foldl(
    fun(#{?SHIP_ID_HEAD := Id, ?SHIP_SIZE_HEAD := Size}, Acc) ->
      [{Id, Size} | Acc]
    end, [], Ships).

%% @private
notify_turn(P1, P2, N) ->
  notify_active_player(P1, P2, 1 - N, turn),
  notify_active_player(P1, P2, N, enemy_turn).

%% @private
notify_active_player(Pid1, _, 0, Notification) -> notify_player(Pid1, Notification);
notify_active_player(_, Pid2, 1, Notification) -> notify_player(Pid2, Notification).

%% @private %TODO send messages format to players?
notify_player(Pid, Notification) -> Pid ! Notification.