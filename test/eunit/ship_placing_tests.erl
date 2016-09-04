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