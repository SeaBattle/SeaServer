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

-include("ss_game.hrl").
-include("ss_codes.hrl").

%% API
-export([set_fleet/3]).

set_fleet(Ships, Pid,
    State = #game_state{player1 = {P1, _}, player2 = {P2, _}, rules = Rules}) when Pid =:= P1; Pid =:= P2 ->
  #game_state{fleet_player1 = F1, fleet_player2 = F2} = State,
  try ss_map_logic:place_ships(Ships, Rules) of
    Map when Pid =:= P1, F2 =:= [] ->  %fleet belongs to player1, player2 fleet is empty
      {prepare, true, State#game_state{fleet_player1 = Map}};
    Map when Pid =:= P2, F1 =:= [] ->  %fleet belongs to player2, player1 fleet is empty
      {prepare, true, State#game_state{fleet_player2 = Map}};
    Map when Pid =:= P1 ->  %fleet belongs to player1, player2 fleet was already set. Player2 will be first to fire. Start the game
      UState = State#game_state{fleet_player1 = Map, active = 1},
      start_game(UState),
      {prepare, true, UState};
    Map when Pid =:= P2 ->  %fleet belongs to player2, player1 fleet was already set. Player1 will be first to fire. Start the game
      UState = State#game_state{fleet_player2 = Map, active = 0},
      start_game(UState),
      {prepare, true, UState}
  catch throw:{error, Code} -> {error, Code}
  end;
set_fleet(_, _, State) -> %fleet package was got from unknown player. May be wrong game.
  {prepare, {error, ?WRONG_GAME}, State}.


%% @private
start_game(#game_state{player1 = {P1, _}, player2 = {P2, _}, active = ActiveNum}) ->  %TODO send messages to players
  P1 ! start,
  P2 ! start,
  case ActiveNum of
    0 ->
      P1 ! turn,
      P2 ! enemy_turn;
    1 ->
      P2 ! turn,
      P1 ! enemy_turn
  end.