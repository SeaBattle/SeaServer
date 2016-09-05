%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2016 12:56
%%%-------------------------------------------------------------------
-module(ship_placing_tests).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

check_horizontal_begin_test() ->
  Line = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
  X = 0,
  Length = 4,
  ?assert(ss_ship_logic:check_horizont(Line, X, Length)).

check_horizontal_end_test() ->
  Line = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
  X = 5,
  Length = 4,
  ?assert(ss_ship_logic:check_horizont(Line, X, Length)).

check_horizontal_out_of_bounds_test() ->
  Line = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
  Length = 4,
  ?assertNot(ss_ship_logic:check_horizont(Line, 10, Length)),
  ?assertNot(ss_ship_logic:check_horizont(Line, 7, Length)).

check_horizontal_other_ship_test() ->
  Line = <<0, 0, 0, 0, 0, 5, 5, 5, 0, 0>>,
  Length = 4,
  ?assert(ss_ship_logic:check_horizont(Line, 0, Length)),
  ?assertNot(ss_ship_logic:check_horizont(Line, 3, Length)),
  ?assertNot(ss_ship_logic:check_horizont(Line, 8, Length)).

check_vertical_begin_test() ->
  Map = ss_ship_logic:empty_map(),
  X = 0,
  Y = 0,
  Size = 4,
  ?assert(ss_ship_logic:check_vertical(X, Y, Size, Map)).

check_vertical_end_test() ->
  Map = ss_ship_logic:empty_map(),
  X = 0,
  Y = 5,
  Size = 4,
  ?assert(ss_ship_logic:check_vertical(X, Y, Size, Map)).

check_vertical_out_of_bounds_test() ->
  Map = ss_ship_logic:empty_map(),
  Size = 4,
  ?assertNot(ss_ship_logic:check_vertical(0, 7, Size, Map)).

check_vertical_other_ship_test() ->
  Map =
  #{
    0 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    1 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
    2 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
    3 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
    4 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    5 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    6 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    7 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    8 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    9 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>
  },
  Size = 4,
  ?assert(ss_ship_logic:check_vertical(4, 1, Size, Map)),
  ?assertNot(ss_ship_logic:check_vertical(3, 1, Size, Map)),
  ?assertNot(ss_ship_logic:check_vertical(3, 3, Size, Map)).