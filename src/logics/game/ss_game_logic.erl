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

-ifdef(TEST).
-compile(export_all).
-endif.

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
%% update players ships. If no ships left - end game. %TODO make game_state use map for fleet and ships of players to avoid code duplication
fire(Shot = #{?FIRE_TYPE_HEAD := X, ?SHOT_HEAD := Shots}, Pid, State = #game_state{shots_left = M}) when ?NORMAL(X), length(Shots) =:= M ->
  case State of
    #game_state{player1 = {Pid, _}, player2 = {Pid2, _}, active = 0} -> %turn of player1 and he makes it
      #game_state{fleet_player2 = Map2, ships_player2 = Ships2} = State,
      {NextState, Result, UMap, UShips, UState} = do_fire(Shot, Shots, Map2, Ships2, Pid2, State),
      {NextState, Result, UState#game_state{fleet_player2 = UMap, ships_player2 = UShips}};
    #game_state{player2 = {Pid, _}, player1 = {Pid1, _}, active = 1} -> %turn of player2 and he makes it
      #game_state{fleet_player1 = Map1, ships_player1 = Ships1} = State,
      {NextState, Result, UMap, UShips, UState} = do_fire(Shot, Shots, Map1, Ships1, Pid1, State),
      {NextState, Result, UState#game_state{fleet_player1 = UMap, ships_player1 = UShips}};
    _ ->
      {play, {error, ?WRONG_TURN}, State}
  end;
fire(Fire, Pid, State) ->
  case maps:is_key(?FIRE_TYPE_HEAD, Fire) of
    false -> fire(Fire#{?FIRE_TYPE_HEAD => ?NORMAL_TYPE}, Pid, State);  %default fire type is normal
    true -> {play, {error, ?BAD_PACKAGE}, State}
  end.

%% @private
do_fire(Shot, Shots, Map, Ships, Enemy, State) -> %TODO proper return format for players  %TODO remove state
  case do_fire_cycle(Shots, Map, Ships) of
    {Hits, UMap, []} -> %all ships ended - end game
      notify_player(Enemy, Shot#{?ACTION_HEAD => fire, ?PAYLOAD_HEAD => #{?ACTION_HEAD => loose}}),
      {end_game, {Hits, win}, UMap, [], State};
    {Hits, UMap, UShips} ->
      {Change, UState} = change_turn(State, get_hits_num(UShips)),
      notify_player(Enemy, Shot#{?ACTION_HEAD => fire, ?CHANGE_TURN_HEAD => Change}),
      {play, {Hits, {change, Change}}, UMap, UShips, UState}
  end.

%% @private
do_fire_cycle(Shots, Map, Ships) ->
  lists:foldl(
    fun(#{?FIRE_X_HEAD := X, ?FIRE_Y_HEAD := Y}, {Hits, UMap, UShips}) ->
      do_one_shot(UMap, X, Y, UShips, Hits)
    end, {[], Map, Ships}, Shots).

%% @private
do_one_shot(Map, X, Y, Ships, Hits) ->
  case mark_fire(Map, X, Y, Ships) of
    {{kill, Id, []}, UMap2} ->   %ship was killed (no more ships for player) -> end game
      {[{X, Y, kill, Id} | Hits], UMap2, []};
    {{Hit, Id, UShips2}, UMap2} when Hit =:= kill; Hit =:= hit ->   %ship was hit or killed
      {[{X, Y, Hit, Id} | Hits], UMap2, UShips2};
    {{miss, _}, UMap2} ->  %miss, change turn
      {[{X, Y, miss} | Hits], UMap2, Ships}
  end.

%% @private
get_hits_num(Shots) ->
  length(lists:filter(
    fun
      ({_, _, miss}) -> false;
      (_) -> true
    end, Shots)).

%% @private
-spec change_turn(#game_state{}, integer()) -> {boolean(), #game_state{}}.
change_turn(State = #game_state{rules = #{?FIRES_PER_TURN_HEAD := M}, active = N}, 0) ->   %no hits. Change turn
  {true, State#game_state{active = 1 - N, shots_left = M}};
change_turn(State = #game_state{rules = Rules, active = N}, K) when K > 0 ->   %was some hits. Should change turn depends on rules
  case Rules of
    #{?REPEAT_ON_HIT_HEAD := true} -> %repeat turn on hit
      {false, State#game_state{shots_left = K}};
    #{?REPEAT_ON_HIT_HEAD := false, ?FIRES_PER_TURN_HEAD := M} ->  %no repeat on hit - change turn
      {true, State#game_state{active = 1 - N, shots_left = M}}
  end.

%% @private
-spec mark_fire(map(), pos_integer(), pos_integer(), list()) ->
  {{kill | hit, pos_integer(), list()} | {miss, list()}, map()}.
mark_fire(Map, X, Y, Ships) ->
  case ss_map_logic:perform_fire(Map, X, Y) of
    {true, Id, UMap} ->
      {fire_at_ship(Ships, Id), UMap};
    {false, UMap} ->
      {{miss, Ships}, UMap}
  end.

%% @private
-spec fire_at_ship(list(), pos_integer()) -> {kill | hit, pos_integer(), list()}.
fire_at_ship(Ships, N) ->
  case lists:keyfind(N, 1, Ships) of
    {N, 1} -> {kill, N, lists:keydelete(N, 1, Ships)};
    {N, Size} -> {hit, N, lists:keyreplace(N, 1, Ships, {N, Size - 1})}
  end.

%% @private
start_game(#game_state{player1 = {P1, _}, player2 = {P2, _}, active = ActiveNum, game_id = GID}) ->
  notify_player(P1, #{?GAME_ID_HEAD => GID, ?ACTION_HEAD => start}),
  notify_player(P2, #{?GAME_ID_HEAD => GID, ?ACTION_HEAD => start}),
  case ActiveNum of %TODO change this, when go to map game_state
    0 -> notify_turn(P1, P2, GID);
    1 -> notify_turn(P2, P1, GID)
  end.

%% @private
form_ships(Ships) ->
  lists:foldl(
    fun(#{?SHIP_ID_HEAD := Id, ?SHIP_SIZE_HEAD := Size}, Acc) ->
      [{Id, Size} | Acc]
    end, [], Ships).

%% @private
notify_turn(P1, P2, GID) ->
  notify_player(P1, #{?GAME_ID_HEAD => GID, ?ACTION_HEAD => turn}),
  notify_player(P2, #{?GAME_ID_HEAD => GID, ?ACTION_HEAD => enemy_turn}).

%% @private
notify_player(Pid, Notification) -> Pid ! {command, Notification}.