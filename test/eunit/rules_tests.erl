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
-include("ss_headers.hrl").

form_full_rules_test() ->
  Rules =
    #{
      ?NEAR_PLACING_HEAD => true,
      ?REPEAT_ON_HIT_HEAD => false,
      ?FIRES_PER_TURN_HEAD => 5,
      ?RECONNECT_TIMER_HEAD => <<"inf">>,
      ?SHIP_DECK1_HEAD => 0,
      ?SHIP_DECK2_HEAD => 3,
      ?SHIP_DECK3_HEAD => 2,
      ?SHIP_DECK4_HEAD => 1,
      ?SHIP_DECK5_HEAD => 1
    },
  RulesEnc = ss_game_rules:encode_rules(Rules),
  ?assertEqual(Rules, ss_game_rules:decode_rules(RulesEnc)).

form_partial_rules_test() ->
  Rules =
    #{
      ?REPEAT_ON_HIT_HEAD => false,
      ?NEAR_PLACING_HEAD => true,
      ?FIRES_PER_TURN_HEAD => 5
    },
  RulesEnc = ss_game_rules:encode_rules(Rules),
  RulesFull =
    Rules#{?RECONNECT_TIMER_HEAD => <<"5s">>, ?SHIP_DECK1_HEAD => 4, ?SHIP_DECK2_HEAD => 3,
      ?SHIP_DECK3_HEAD => 2, ?SHIP_DECK4_HEAD => 1, ?SHIP_DECK5_HEAD => 0},
  ?assertEqual(RulesFull, ss_game_rules:decode_rules(RulesEnc)).

form_default_rules_test() ->
  Default =
    #{
      ?RECONNECT_TIMER_HEAD => <<"5s">>,
      ?REPEAT_ON_HIT_HEAD => true,
      ?NEAR_PLACING_HEAD => false,
      ?FIRES_PER_TURN_HEAD => 1,
      ?SHIP_DECK1_HEAD => 4,
      ?SHIP_DECK2_HEAD => 3,
      ?SHIP_DECK3_HEAD => 2,
      ?SHIP_DECK4_HEAD => 1,
      ?SHIP_DECK5_HEAD => 0
    },
  DefaultEnc = ss_game_rules:encode_rules(#{}),
  ?assertEqual(Default, ss_game_rules:decode_rules(DefaultEnc)).

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
  Res = try ss_game_rules:encode_rules(Rules)
        catch throw:Err -> Err
        end,
  ?assertEqual({error, ?INCORRECT_SHIP_NUMBER}, Res).