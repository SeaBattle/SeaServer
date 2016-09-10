%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2016 15:09
%%%-------------------------------------------------------------------
-module(ship_placing_tests).
-author("tihon").

-include("ss_headers.hrl").
-include_lib("eunit/include/eunit.hrl").

place_correct_test() ->
  EmptyMap = ss_map_logic:empty_map(),
  MapWithShip = ss_map_logic:place_ship(#{?SHIP_ID_HEAD => 10, ?SHIP_X_POS_HEAD => 2,
    ?SHIP_Y_POS_HEAD => 3, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 3}, EmptyMap, true),
  ?assertEqual(EmptyMap#{3 => <<0, 0, 10, 10, 10, 0, 0, 0, 0, 0>>}, MapWithShip),

  MapWithShip2 = ss_map_logic:place_ship(#{?SHIP_ID_HEAD => 3, ?SHIP_X_POS_HEAD => 5,
    ?SHIP_Y_POS_HEAD => 4, ?SHIP_DIRECTION_HEAD => <<"w">>, ?SHIP_SIZE_HEAD => 4}, MapWithShip, true),
  Expected = MapWithShip#{
    4 => <<0, 0, 0, 0, 0, 3, 0, 0, 0, 0>>,
    5 => <<0, 0, 0, 0, 0, 3, 0, 0, 0, 0>>,
    6 => <<0, 0, 0, 0, 0, 3, 0, 0, 0, 0>>,
    7 => <<0, 0, 0, 0, 0, 3, 0, 0, 0, 0>>
  },
  ?assertEqual(Expected, MapWithShip2),
  ok.

check_n_place_w_test() ->
  Map1 =
    #{
      0 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      1 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      2 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      3 => <<1, 1, 1, 0, 1, 1, 1, 1, 1, 1>>,
      4 => <<1, 1, 1, 0, 1, 1, 1, 1, 1, 1>>,
      5 => <<1, 1, 1, 0, 1, 1, 1, 1, 1, 1>>,
      6 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      7 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      8 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      9 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>
    },
  MapWithShip = ss_map_logic:place_ship(#{?SHIP_ID_HEAD => 3, ?SHIP_X_POS_HEAD => 3,
    ?SHIP_Y_POS_HEAD => 3, ?SHIP_DIRECTION_HEAD => <<"w">>, ?SHIP_SIZE_HEAD => 3}, Map1, true),
  ?assertEqual(
    #{
      0 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      1 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      2 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      3 => <<1, 1, 1, 3, 1, 1, 1, 1, 1, 1>>,
      4 => <<1, 1, 1, 3, 1, 1, 1, 1, 1, 1>>,
      5 => <<1, 1, 1, 3, 1, 1, 1, 1, 1, 1>>,
      6 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      7 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      8 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      9 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>
    },
    MapWithShip),
  ok.

check_n_place_h_test() ->
  Map1 =
    #{
      0 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      1 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      2 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      3 => <<1, 1, 1, 0, 0, 0, 1, 1, 1, 1>>,
      4 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      5 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      6 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      7 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      8 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      9 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>
    },
  MapWithShip = ss_map_logic:place_ship(#{?SHIP_ID_HEAD => 3, ?SHIP_X_POS_HEAD => 3,
    ?SHIP_Y_POS_HEAD => 3, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 3}, Map1, true),
  ?assertEqual(
    #{
      0 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      1 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      2 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      3 => <<1, 1, 1, 3, 3, 3, 1, 1, 1, 1>>,
      4 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      5 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      6 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      7 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      8 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
      9 => <<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>
    },
    MapWithShip),
  ok.

place_complete_test() ->
  Ship1 = #{?SHIP_ID_HEAD => 1, ?SHIP_X_POS_HEAD => 0, ?SHIP_Y_POS_HEAD => 0, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 1},
  Ship2 = #{?SHIP_ID_HEAD => 2, ?SHIP_X_POS_HEAD => 9, ?SHIP_Y_POS_HEAD => 0, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 1},
  Ship3 = #{?SHIP_ID_HEAD => 3, ?SHIP_X_POS_HEAD => 0, ?SHIP_Y_POS_HEAD => 9, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 1},
  Ship4 = #{?SHIP_ID_HEAD => 4, ?SHIP_X_POS_HEAD => 9, ?SHIP_Y_POS_HEAD => 9, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 1},
  Ship5 = #{?SHIP_ID_HEAD => 5, ?SHIP_X_POS_HEAD => 3, ?SHIP_Y_POS_HEAD => 1, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 2},
  Ship6 = #{?SHIP_ID_HEAD => 6, ?SHIP_X_POS_HEAD => 5, ?SHIP_Y_POS_HEAD => 3, ?SHIP_DIRECTION_HEAD => <<"w">>, ?SHIP_SIZE_HEAD => 2},
  Ship7 = #{?SHIP_ID_HEAD => 7, ?SHIP_X_POS_HEAD => 7, ?SHIP_Y_POS_HEAD => 5, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 2},
  Ship8 = #{?SHIP_ID_HEAD => 8, ?SHIP_X_POS_HEAD => 1, ?SHIP_Y_POS_HEAD => 7, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 3},
  Ship9 = #{?SHIP_ID_HEAD => 9, ?SHIP_X_POS_HEAD => 5, ?SHIP_Y_POS_HEAD => 8, ?SHIP_DIRECTION_HEAD => <<"h">>, ?SHIP_SIZE_HEAD => 3},
  Ship10 = #{?SHIP_ID_HEAD => 10, ?SHIP_X_POS_HEAD => 1, ?SHIP_Y_POS_HEAD => 2, ?SHIP_DIRECTION_HEAD => <<"w">>, ?SHIP_SIZE_HEAD => 4},

  Expected =
    #{
      0 => <<1, 0, 0, 0, 0, 0, 0, 0, 0, 2>>,
      1 => <<0, 0, 0, 5, 5, 0, 0, 0, 0, 0>>,
      2 => <<0, 10, 0, 0, 0, 0, 0, 0, 0, 0>>,
      3 => <<0, 10, 0, 0, 0, 6, 0, 0, 0, 0>>,
      4 => <<0, 10, 0, 0, 0, 6, 0, 0, 0, 0>>,
      5 => <<0, 10, 0, 0, 0, 0, 0, 7, 7, 0>>,
      6 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      7 => <<0, 8, 8, 8, 0, 0, 0, 0, 0, 0>>,
      8 => <<0, 0, 0, 0, 0, 9, 9, 9, 0, 0>>,
      9 => <<3, 0, 0, 0, 0, 0, 0, 0, 0, 4>>
    },
  ?assertEqual(Expected,
    ss_map_logic:place_ships([Ship1, Ship2, Ship3, Ship4, Ship5, Ship6, Ship7, Ship8, Ship9, Ship10], #{?NEAR_PLACING_HEAD => true})),
  ok.