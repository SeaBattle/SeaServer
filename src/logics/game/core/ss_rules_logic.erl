%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2016 18:13
%%%-------------------------------------------------------------------
-module(ss_rules_logic).
-author("tihon").

-include("ss_headers.hrl").
-include("ss_codes.hrl").

-define(DEFAULT_PLACING, false).
-define(DEFAULT_RECONNECT_TIMER, <<"5s">>).  %5 seconds
-define(DEFAULT_REPEAT_ON_HIT, true).
-define(DEFAULT_FIRES_PER_TURN, 1).
-define(DEFAULT_1_DECK_SHIPS, 4).
-define(DEFAULT_2_DECK_SHIPS, 3).
-define(DEFAULT_3_DECK_SHIPS, 2).
-define(DEFAULT_4_DECK_SHIPS, 1).
-define(DEFAULT_5_DECK_SHIPS, 0).

%% API
-export([encode_rules/1, decode_rules/1]).

-spec encode_rules(map()) -> binary().
encode_rules(Rules) ->
  Placing = get_allow_near_placing(Rules),
  Reconnect = get_reconnect_timer(Rules),
  Repeat = get_repeat_turn_on_hit(Rules),
  Fires = get_fires_per_turn(Rules),
  Ships = get_ships_sizes(Rules),
  case lists:sum(Ships) of
    0 -> throw({error, ?INCORRECT_SHIP_NUMBER});
    _ -> ok
  end,
  compile_rules(Placing, Repeat, Fires, Ships, Reconnect).

-spec decode_rules(binary()) -> map().
decode_rules(RulesBin) ->
  List = binary_to_term(RulesBin),
  [Placing, Repeat, Fires, Ships, Reconnect] = List,
  <<Deck1, Deck2, Deck3, Deck4, Deck5>> = Ships,
  #{
    ?RECONNECT_TIMER_HEAD => Reconnect,
    ?REPEAT_ON_HIT_HEAD => Repeat =:= 1,
    ?NEAR_PLACING_HEAD => Placing =:= 1,
    ?FIRES_PER_TURN_HEAD => Fires,
    ?SHIP_DECK1_HEAD => Deck1,
    ?SHIP_DECK2_HEAD => Deck2,
    ?SHIP_DECK3_HEAD => Deck3,
    ?SHIP_DECK4_HEAD => Deck4,
    ?SHIP_DECK5_HEAD => Deck5
  }.


%% @private
get_reconnect_timer(#{?RECONNECT_TIMER_HEAD := B}) when is_binary(B) -> B;
get_reconnect_timer(_) -> ?DEFAULT_RECONNECT_TIMER. %TODO forbid infinity timer

%% @private
get_allow_near_placing(#{?NEAR_PLACING_HEAD := B}) when is_boolean(B) -> B;
get_allow_near_placing(_) -> ?DEFAULT_PLACING.

%% @private
get_repeat_turn_on_hit(#{?REPEAT_ON_HIT_HEAD := B}) when is_boolean(B) -> B;
get_repeat_turn_on_hit(_) -> ?DEFAULT_REPEAT_ON_HIT.

%% @private
get_fires_per_turn(#{?FIRES_PER_TURN_HEAD := N}) when is_integer(N) -> N;
get_fires_per_turn(_) -> ?DEFAULT_FIRES_PER_TURN.

%% @private
get_ships_sizes(Map) ->
  Deck1 = get_ship_size(Map, ?SHIP_DECK1_HEAD, ?DEFAULT_1_DECK_SHIPS),
  Deck2 = get_ship_size(Map, ?SHIP_DECK2_HEAD, ?DEFAULT_2_DECK_SHIPS),
  Deck3 = get_ship_size(Map, ?SHIP_DECK3_HEAD, ?DEFAULT_3_DECK_SHIPS),
  Deck4 = get_ship_size(Map, ?SHIP_DECK4_HEAD, ?DEFAULT_4_DECK_SHIPS),
  Deck5 = get_ship_size(Map, ?SHIP_DECK5_HEAD, ?DEFAULT_5_DECK_SHIPS),
  [Deck1, Deck2, Deck3, Deck4, Deck5].

%% @private
get_ship_size(Map, Type, Default) ->
  case maps:find(Type, Map) of
    {ok, Value} when is_integer(Value), Value >= 0 -> Value;
    _ -> Default
  end.

%% @private
-spec compile_rules(boolean(), boolean(), integer(), list(integer()), binary()) -> binary().
compile_rules(Placing, Repeat, Fires, Ships, Reconnect) ->
  All = [Placing | [Repeat | [Fires | [Ships | [Reconnect]]]]],
  Prepared = lists:map(fun binarize_rule/1, All),
  term_to_binary(Prepared).

%% @private
binarize_rule(true) -> 1;
binarize_rule(false) -> 0;
binarize_rule(List) when is_list(List) -> list_to_binary(List);
binarize_rule(Other) -> Other.