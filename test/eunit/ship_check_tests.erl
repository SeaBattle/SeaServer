%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2016 12:56
%%%-------------------------------------------------------------------
-module(ship_check_tests).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").


cut_h_test() ->
  Map =
    #{
      0 => <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9>>,
      1 => <<1, 1, 2, 3, 4, 5, 6, 7, 8, 9>>,
      2 => <<2, 2, 2, 2, 2, 2, 2, 2, 2, 2>>,
      3 => <<3, 3, 3, 3, 3, 3, 3, 3, 3, 3>>,
      4 => <<4, 4, 4, 4, 4, 4, 4, 4, 4, 4>>,
      5 => <<5, 5, 5, 5, 5, 5, 5, 5, 5, 5>>,
      6 => <<6, 6, 6, 6, 6, 6, 6, 6, 6, 6>>,
      7 => <<7, 7, 7, 7, 7, 7, 7, 7, 7, 7>>,
      8 => <<8, 8, 8, 8, 8, 8, 8, 8, 8, 8>>,
      9 => <<9, 9, 9, 9, 9, 9, 9, 9, 9, 9>>
    },
  ?assertEqual([<<0, 1, 2, 3>>], ss_ship_logic:cut_space(Map, 0, 0, 4, <<"h">>, true)),
  ?assertEqual([<<0>>], ss_ship_logic:cut_space(Map, 0, 0, 1, <<"h">>, true)),
  ?assertEqual([<<6, 7, 8, 9>>], ss_ship_logic:cut_space(Map, 6, 1, 4, <<"h">>, true)),
  ?assertEqual([], ss_ship_logic:cut_space(Map, 7, 1, 4, <<"h">>, true)),

  ?assertEqual(
    [
      <<1, 1, 2, 3, 4>>,
      <<0, 1, 2, 3, 4>>
    ], ss_ship_logic:cut_space(Map, 0, 0, 4, <<"h">>, false)),
  ?assertEqual(
    [
      <<1, 1>>,
      <<0, 1>>
    ], ss_ship_logic:cut_space(Map, 0, 0, 1, <<"h">>, false)),
  ?assertEqual(
    [
      <<6, 6, 6>>,
      <<5, 5, 5>>,
      <<4, 4, 4>>
    ], ss_ship_logic:cut_space(Map, 5, 5, 1, <<"h">>, false)),
  ?assertEqual(
    [
      <<2, 2, 2, 2, 2>>,
      <<5, 6, 7, 8, 9>>,
      <<5, 6, 7, 8, 9>>
    ], ss_ship_logic:cut_space(Map, 6, 1, 4, <<"h">>, false)),
  ?assertEqual([], ss_ship_logic:cut_space(Map, 7, 1, 4, <<"h">>, false)),
  ok.

cut_w_test() ->
  Map =
    #{
      0 => <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9>>,
      1 => <<1, 1, 2, 3, 4, 5, 6, 7, 8, 9>>,
      2 => <<2, 2, 2, 2, 2, 2, 2, 2, 2, 2>>,
      3 => <<3, 3, 3, 3, 3, 3, 3, 3, 3, 3>>,
      4 => <<4, 4, 4, 4, 4, 4, 4, 4, 4, 4>>,
      5 => <<5, 5, 5, 5, 5, 5, 5, 5, 5, 5>>,
      6 => <<6, 6, 6, 6, 6, 6, 6, 6, 6, 6>>,
      7 => <<7, 7, 7, 7, 7, 7, 7, 7, 7, 7>>,
      8 => <<8, 8, 8, 8, 8, 8, 8, 8, 8, 8>>,
      9 => <<9, 9, 9, 9, 9, 9, 9, 9, 9, 9>>
    },
  ?assertEqual([0, 1, 2, 3], ss_ship_logic:cut_space(Map, 0, 0, 4, <<"w">>, true)),
  ?assertEqual([0], ss_ship_logic:cut_space(Map, 0, 0, 1, <<"w">>, true)),
  ?assertEqual([6, 2, 3, 4], ss_ship_logic:cut_space(Map, 6, 1, 4, <<"w">>, true)),
  ?assertEqual([], ss_ship_logic:cut_space(Map, 1, 7, 4, <<"w">>, true)),

  ?assertEqual([1, 0, 1, 1, 2, 2, 3, 3, 4, 4], ss_ship_logic:cut_space(Map, 0, 0, 4, <<"w">>, false)),
  ?assertEqual([1, 0, 1, 1], ss_ship_logic:cut_space(Map, 0, 0, 1, <<"w">>, false)),
  ?assertEqual([4, 4, 4, 5, 5, 5, 6, 6, 6], ss_ship_logic:cut_space(Map, 5, 5, 1, <<"w">>, false)),
  ?assertEqual(
    [5, 7, 6, 5, 7, 6, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5], ss_ship_logic:cut_space(Map, 6, 1, 4, <<"w">>, false)),
  ?assertEqual([], ss_ship_logic:cut_space(Map, 1, 7, 4, <<"w">>, false)),
  ok.

can_be_placed_near_test() ->
  Map =
    #{
      0 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      1 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
      2 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
      3 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
      4 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      5 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      6 => <<4, 0, 0, 0, 0, 3, 3, 3, 3, 0>>,
      7 => <<4, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      8 => <<4, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      9 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>
    },
  ?assert(ss_ship_logic:can_be_placed(0, 0, <<"h">>, 3, Map, true)),
  ?assert(ss_ship_logic:can_be_placed(1, 6, <<"h">>, 3, Map, true)),
  ?assert(ss_ship_logic:can_be_placed(2, 6, <<"h">>, 3, Map, true)),
  ?assertNot(ss_ship_logic:can_be_placed(3, 6, <<"h">>, 3, Map, true)),
  ?assert(ss_ship_logic:can_be_placed(4, 0, <<"w">>, 3, Map, true)),
  ?assert(ss_ship_logic:can_be_placed(5, 3, <<"w">>, 3, Map, true)),
  ?assertNot(ss_ship_logic:can_be_placed(5, 4, <<"w">>, 3, Map, true)).

can_be_placed_test() ->
  Map =
    #{
      0 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      1 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
      2 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
      3 => <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0>>,
      4 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      5 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      6 => <<4, 0, 0, 0, 0, 3, 3, 3, 3, 0>>,
      7 => <<4, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      8 => <<4, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
      9 => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>
    },
  ?assert(ss_ship_logic:can_be_placed(2, 6, <<"h">>, 2, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(1, 6, <<"h">>, 2, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(4, 5, <<"h">>, 3, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(5, 5, <<"h">>, 3, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(4, 3, <<"h">>, 3, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(4, 4, <<"h">>, 3, Map, false)),
  ?assert(ss_ship_logic:can_be_placed(5, 4, <<"h">>, 3, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(5, 3, <<"w">>, 3, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(1, 3, <<"w">>, 3, Map, false)),
  ?assert(ss_ship_logic:can_be_placed(1, 2, <<"w">>, 3, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(1, 6, <<"w">>, 3, Map, false)),
  ?assert(ss_ship_logic:can_be_placed(5, 3, <<"w">>, 2, Map, false)),
  ?assertNot(ss_ship_logic:can_be_placed(5, 4, <<"w">>, 3, Map, false)).