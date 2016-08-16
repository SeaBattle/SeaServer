%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2016 18:13
%%%-------------------------------------------------------------------
-module(ss_game_rules).
-author("tihon").

-include("ss_codes.hrl").

-define(DEFAULT_PLACING, false).
-define(DEFAULT_REPEAT_ON_HIT, true).
-define(DEFAULT_FIRES_PER_TURN, 1).
-define(DEFAULT_1_DECK_SHIPS, 4).
-define(DEFAULT_2_DECK_SHIPS, 3).
-define(DEFAULT_3_DECK_SHIPS, 2).
-define(DEFAULT_4_DECK_SHIPS, 1).
-define(DEFAULT_5_DECK_SHIPS, 0).

%% API
-export([form_rules/1]).

-spec form_rules(map()) -> binary().
form_rules(Rules) ->
  Placing = get_allow_near_placing(Rules),
  Repeat = get_repeat_turn_on_hit(Rules),
  Fires = get_fires_per_turn(Rules),
  Ships = get_ships_sizes(Rules),
  case lists:sum(Ships) of
    0 -> throw({error, ?INCORRECT_SHIP_NUMBER});
    _ -> ok
  end,
  compile_rules(Placing, Repeat, Fires, Ships).


%% @private
get_allow_near_placing(#{<<"allow_near_placing">> := B}) when is_boolean(B) -> B;
get_allow_near_placing(_) -> ?DEFAULT_PLACING.

%% @private
get_repeat_turn_on_hit(#{<<"repeat_turn_on_hit">> := B}) when is_boolean(B) -> B;
get_repeat_turn_on_hit(_) -> ?DEFAULT_REPEAT_ON_HIT.

%% @private
get_fires_per_turn(#{<<"fires_per_turn">> := N}) when is_integer(N) -> N;
get_fires_per_turn(_) -> ?DEFAULT_FIRES_PER_TURN.

%% @private
get_ships_sizes(Map) ->
  Deck1 = get_ship_size(Map, <<"deck1">>, ?DEFAULT_1_DECK_SHIPS),
  Deck2 = get_ship_size(Map, <<"deck2">>, ?DEFAULT_2_DECK_SHIPS),
  Deck3 = get_ship_size(Map, <<"deck3">>, ?DEFAULT_3_DECK_SHIPS),
  Deck4 = get_ship_size(Map, <<"deck4">>, ?DEFAULT_4_DECK_SHIPS),
  Deck5 = get_ship_size(Map, <<"deck5">>, ?DEFAULT_5_DECK_SHIPS),
  [Deck1, Deck2, Deck3, Deck4, Deck5].

%% @private
get_ship_size(Map, Type, Default) ->
  case maps:find(Type, Map) of
    {ok, Value} when is_integer(Value), Value >= 0 -> Value;
    _ -> Default
  end.

%% @private
-spec compile_rules(boolean(), boolean(), integer(), list(integer())) -> binary().
compile_rules(Placing, Repeat, Fires, Ships) ->
  All = [Placing | [Repeat | [Fires | Ships]]],
  lists:foldl(
    fun
      (true, Acc) -> <<Acc/binary, 1>>;
      (false, Acc) -> <<Acc/binary, 0>>;
      (Int, Acc) when is_integer(Int) -> <<Acc/binary, Int>>
    end, <<>>, All).
