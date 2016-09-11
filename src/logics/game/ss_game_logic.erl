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
fire(#{?FIRE_TYPE := ?NORMAL_TYPE, ?FIRE_X := X, ?FIRE_Y := Y}, Pid, State) when ?PROPER(X, Y) -> %ordinary fire
  case State of
    #game_state{player1 = {Pid, _}, player2 = {Pid2, _}, fleet_player2 = Map2, ships_player2 = Ships2, active = 0} -> %turn of player1 and he makes it
      notify_player(Pid2, {fire, ?NORMAL_TYPE, X, Y}),
      do_one_shot(Map2, X, Y, Ships2, State);
    #game_state{player2 = {Pid, _}, player1 = {Pid1, _}, fleet_player1 = Map1, ships_player1 = Ships1, active = 1} -> %turn of player2 and he makes it
      notify_player(Pid1, {fire, ?NORMAL_TYPE, X, Y}),
      do_one_shot(Map1, X, Y, Ships1, State);
    _ ->
      {play, {error, ?WRONG_TURN}, State}
  end,
  ok;
%TODO NORMAL_MULTI_TYPE?
%TODO repeat turn on multi hit
fire(Fire, Pid, State) ->
  case maps:is_key(?FIRE_TYPE, Fire) of
    false -> fire(Fire#{?FIRE_TYPE => ?NORMAL_TYPE}, Pid, State);  %default fire type is normal
    true -> {play, {error, ?BAD_PACKAGE}, State}
  end.

%% @private
do_one_shot(Map, X, Y, Ships, State = #game_state{player1 = {P1, _}, player2 = {P2, _}, active = N}) ->
  case mark_fire(Map, X, Y, Ships) of
    {true, [], UMap2} ->   %ship was hit (no more ships for player)
      notify_active_player(P1, P2, N, win),
      notify_active_player(P1, P2, 1 - N, loose),
      {end_game, true, State#game_state{fleet_player2 = UMap2, ships_player2 = []}};
    {true, UShips2, UMap2} ->   %ship was hit
      UState = change_turn_on_hit(UShips2, State),
      {play, true, UState#game_state{fleet_player2 = UMap2, ships_player2 = UShips2}};
    {false, UMap2} ->  %miss, change turn
      UN = 1 - N,
      notify_active_player(P1, P2, UN, turn),
      notify_active_player(P1, P2, 1 - UN, enemy_turn),
      {play, false, State#game_state{fleet_player2 = UMap2, active = UN}}
  end.

%% @private
change_turn_on_hit([], _) -> endGame; %no more enemy ships left - end game
change_turn_on_hit(_, State = #game_state{rules = #{?REPEAT_ON_HIT_HEAD := true}}) -> %repeat on hit - do another fire
  #game_state{player1 = {P1, _}, player2 = {P2, _}, active = N} = State,
  notify_active_player(P1, P2, N, turn),
  State;
change_turn_on_hit(_, State = #game_state{rules = #{?REPEAT_ON_HIT_HEAD := false}, active = N}) ->  %no repeat on hit - turn changes
  State#game_state{active = 1 - N}.

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
notify_active_player(Pid1, _, 0, Notification) -> notify_player(Pid1, Notification);
notify_active_player(_, Pid2, 1, Notification) -> notify_player(Pid2, Notification).

%% @private %TODO send messages format to players?
notify_player(Pid, Notification) -> Pid ! Notification.