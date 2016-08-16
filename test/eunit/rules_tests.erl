%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2016 18:17
%%%-------------------------------------------------------------------
-module(rules_tests).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").
-include("ss_codes.hrl").

form_full_rules_test() ->
  Rules =
    #{
      <<"allow_near_placing">> => true,
      <<"repeat_turn_on_hit">> => false,
      <<"fires_per_turn">> => 5,
      <<"deck1">> => 0,
      <<"deck2">> => 3,
      <<"deck3">> => 2,
      <<"deck4">> => 1,
      <<"deck5">> => 1
    },
  ?assertEqual(<<1, 0, 5, 0, 3, 2, 1, 1>>, ss_game_rules:form_rules(Rules)).

form_partial_rules_test() ->
  Rules =
    #{
      <<"allow_near_placing">> => true,
      <<"repeat_turn_on_hit">> => false,
      <<"fires_per_turn">> => 5
    },
  ?assertEqual(<<1, 0, 5, 4, 3, 2, 1, 0>>, ss_game_rules:form_rules(Rules)).

form_default_rules_test() ->
  ?assertEqual(<<0, 1, 1, 4, 3, 2, 1, 0>>, ss_game_rules:form_rules(#{})).

incorrect_ship_num_test() ->
  Rules =
    #{
      <<"allow_near_placing">> => true,
      <<"repeat_turn_on_hit">> => false,
      <<"fires_per_turn">> => 5,
      <<"deck1">> => 0,
      <<"deck2">> => 0,
      <<"deck3">> => 0,
      <<"deck4">> => 0,
      <<"deck5">> => 0
    },
  Res = try ss_game_rules:form_rules(Rules)
        catch throw:Err -> Err
        end,
  ?assertEqual({error, ?INCORRECT_SHIP_NUMBER}, Res).