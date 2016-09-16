%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2016 11:42
%%%-------------------------------------------------------------------
-module(fires_tests).
-author("tihon").

-include("ss_game.hrl").
-include("ss_headers.hrl").
-include_lib("eunit/include/eunit.hrl").

fire_at_ship_test() ->
  ?assertEqual({hit, 3, [{2, 4}, {3, 2}, {5, 1}]}, ss_game_logic:fire_at_ship([{2, 4}, {3, 3}, {5, 1}], 3)),
  ?assertEqual({kill, 3, [{2, 4}, {5, 1}]}, ss_game_logic:fire_at_ship([{2, 4}, {3, 1}, {5, 1}], 3)).

make_fire_test() ->
  EmptyMap = ss_map_logic:empty_map(),
  MapWithShip = ss_map_logic:place_ship(#{?SHIP_ID_HEAD => 10, ?SHIP_X_POS_HEAD => 2,
    ?SHIP_Y_POS_HEAD => 3, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 2}, EmptyMap, true),
%%  miss
  {Res, UMap} = ss_game_logic:mark_fire(MapWithShip, 4, 4, [{10, 2}]),
  ?assertEqual({miss, [{10, 2}]}, Res),
  ?assertEqual(<<0, 0, 0, 0, 255, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(4, UMap)),
%%  hit
  {Res1, UMap1} = ss_game_logic:mark_fire(MapWithShip, 2, 3, [{10, 2}]),
  ?assertEqual({hit, 10, [{10, 1}]}, Res1),
  ?assertEqual(<<0, 0, 245, 10, 0, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(3, UMap1)),
%%  kill
  {Res2, UMap2} = ss_game_logic:mark_fire(UMap1, 3, 3, [{10, 1}]),
  ?assertEqual({kill, 10, []}, Res2),
  ?assertEqual(<<0, 0, 245, 245, 0, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(3, UMap2)).

one_shot_test() ->
  EmptyMap = ss_map_logic:empty_map(),
  MapWithShip = ss_map_logic:place_ship(#{?SHIP_ID_HEAD => 10, ?SHIP_X_POS_HEAD => 2,
    ?SHIP_Y_POS_HEAD => 3, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 2}, EmptyMap, true),
%%  miss
  {Hits1, UMap, Ships1} = ss_game_logic:do_one_shot(MapWithShip, 4, 4, [{10, 2}], []),
  ?assertEqual([{10, 2}], Ships1),
  ?assertEqual([{4, 4, miss}], Hits1),
  ?assertEqual(<<0, 0, 0, 0, 255, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(4, UMap)),
%%  hit
  {Hits2, UMap1, Ships2} = ss_game_logic:do_one_shot(UMap, 2, 3, Ships1, Hits1),
  ?assertEqual([{10, 1}], Ships2),
  ?assertEqual([{2, 3, hit, 10}, {4, 4, miss}], Hits2),
  ?assertEqual(<<0, 0, 245, 10, 0, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(3, UMap1)),
%%  kill
  {Hits3, UMap2, Ships3} = ss_game_logic:do_one_shot(UMap1, 3, 3, Ships2, Hits2),
  ?assertEqual([], Ships3),
  ?assertEqual([{3, 3, kill, 10}, {2, 3, hit, 10}, {4, 4, miss}], Hits3),
  ?assertEqual(<<0, 0, 245, 245, 0, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(3, UMap2)).

multiple_shots_test() ->
  EmptyMap = ss_map_logic:empty_map(),
  MapWithShip = ss_map_logic:place_ship(#{?SHIP_ID_HEAD => 10, ?SHIP_X_POS_HEAD => 2,
    ?SHIP_Y_POS_HEAD => 3, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 3}, EmptyMap, true),
%%  all miss
  Shots1 =
    [
      #{?FIRE_X_HEAD => 4, ?FIRE_Y_HEAD => 4},
      #{?FIRE_X_HEAD => 5, ?FIRE_Y_HEAD => 5},
      #{?FIRE_X_HEAD => 6, ?FIRE_Y_HEAD => 6}
    ],
  {Hits1, Map1, Ships1} = ss_game_logic:do_fire_cycle(Shots1, MapWithShip, [{10, 3}]),
  ?assertEqual([{6, 6, miss}, {5, 5, miss}, {4, 4, miss}], Hits1),
  ?assertEqual([{10, 3}], Ships1),
  ?assertEqual(<<0, 0, 0, 0, 255, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(4, Map1)),
  ?assertEqual(<<0, 0, 0, 0, 0, 255, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(5, Map1)),
  ?assertEqual(<<0, 0, 0, 0, 0, 0, 255, 0, 0, 0>>, ss_map_logic:get_horizont_line(6, Map1)),
%%  1 hit, 2 miss
  Shots2 =
    [
      #{?FIRE_X_HEAD => 2, ?FIRE_Y_HEAD => 3},
      #{?FIRE_X_HEAD => 2, ?FIRE_Y_HEAD => 2},
      #{?FIRE_X_HEAD => 2, ?FIRE_Y_HEAD => 4}
    ],
  {Hits2, Map2, Ships2} = ss_game_logic:do_fire_cycle(Shots2, Map1, [{10, 3}]),
  ?assertEqual([{2, 4, miss}, {2, 2, miss}, {2, 3, hit, 10}], Hits2),
  ?assertEqual([{10, 2}], Ships2),
  ?assertEqual(<<0, 0, 255, 0, 0, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(2, Map2)),
  ?assertEqual(<<0, 0, 245, 10, 10, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(3, Map2)),
  ?assertEqual(<<0, 0, 255, 0, 255, 0, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(4, Map2)),
  ?assertEqual(<<0, 0, 0, 0, 0, 255, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(5, Map2)),
  ?assertEqual(<<0, 0, 0, 0, 0, 0, 255, 0, 0, 0>>, ss_map_logic:get_horizont_line(6, Map2)),
%% 2 hit, 1 miss
  Shots3 =
    [
      #{?FIRE_X_HEAD => 3, ?FIRE_Y_HEAD => 3},
      #{?FIRE_X_HEAD => 4, ?FIRE_Y_HEAD => 3},
      #{?FIRE_X_HEAD => 5, ?FIRE_Y_HEAD => 3}
    ],
  {Hits3, Map3, Ships3} = ss_game_logic:do_fire_cycle(Shots3, Map2, [{10, 2}]),
  ?assertEqual([{5, 3, miss}, {4, 3, kill, 10}, {3, 3, hit, 10}], Hits3),
  ?assertEqual([], Ships3),
  ?assertEqual(<<0, 0, 245, 245, 245, 255, 0, 0, 0, 0>>, ss_map_logic:get_horizont_line(3, Map3)).