%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2016 17:43
%%%-------------------------------------------------------------------
-module(ss_ship_logic).
-author("tihon").

-include("ss_headers.hrl").
-include("ss_codes.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-define(PROPER(A, B), A > 0, A =< 10, B > 0, B =< 10).
-define(CHECK_SHIP_X(Line, X), 0 =:= binary:at(Line, X)).

%% API
-export([place_ships/2]).

place_ships(Ships, #{?NEAR_PLACING_HEAD := AllowNear}) ->
  EmptyMap = empty_map(),
  lists:foldl(fun(Ship, Map) -> place_ship(Ship, Map, AllowNear) end, EmptyMap, Ships).


%% @private
place_ship(#{?SHIP_ID_HEAD := Id, ?SHIP_X_POS_HEAD := X,
  ?SHIP_Y_POS_HEAD := Y, ?SHIP_DIRECTION_HEAD := D, ?SHIP_SIZE_HEAD := Size}, Map, Near) when ?PROPER(X, Y) ->
  case can_be_placed(X, Y, D, Size, Near, Map) of
    true -> do_place_ship(Id, X, Y, D, Size, Map);
    false -> throw({error, ?INCORRECT_SHIP_POSITION})
  end.

%% @private
can_be_placed(X, Y, <<"h">>, Size, Map, Near) ->
  HorizontAfter = get_horizont_line(Y + 1, Map),
  HorizontShip = get_horizont_line(Y, Map),
  HorizontBefore = get_horizont_line(Y - 1, Map),
  case check_horizont(HorizontShip, X, Size) of
    true when Near =:= true -> true;
    true when Near =:= false ->
      check_horizont(HorizontAfter, X, Size) andalso check_horizont(HorizontBefore, X, Size);
    false -> false
  end;
can_be_placed(X, Y, <<"w">>, Size, Map, Near) when Y + Size =< 10 ->
  case check_vertical(X, Y, Size, Map) of
    true when Near =:= true -> true;
    true when Near =:= false ->
      check_vertical(X, Y + 1, Size, Map) andalso check_vertical(X, Y - 1, Size, Map);
    false -> false
  end.

%% @private
do_place_ship(Id, X, Y, <<"h">>, Size, Map) ->
  HorizontShip = get_horizont_line(Y, Map),
  UpdatedLine = ss_utils:replace_at(HorizontShip, X, <<<<Id>> || <<_:1>> <= <<0:Size>>>>),
  Map#{X => UpdatedLine};
do_place_ship(Id, X, Y, <<"w">>, Size, Map) ->
  lists:foldl(
    fun(N, Acc) ->
      K = X + N,
      Line = get_horizont_line(K, Acc),
      Acc#{K => ss_utils:replace_at(Line, Y, <<Id>>)}
    end, Map, lists:seq(0, Size)).

%% @private
check_horizont(undefined, _, _) -> true;
check_horizont(Line, X, Length) when is_binary(Line), X + Length < 10 ->
  lists:foldl(
    fun
      (_, false) -> false;
      (XN, _) -> ?CHECK_SHIP_X(Line, XN)
    end, ?CHECK_SHIP_X(Line, X), lists:seq(1, Length - 1));
check_horizont(_, _, _) -> false.

%% @private
check_vertical(_, Y, Size, _) when Y + Size >= 10 -> false;
check_vertical(X, Y, Size, Map) ->
  lists:foldl(
    fun
      (_, false) -> false;
      (N, _) ->
        K = X + N,
        Line = get_horizont_line(K, Map),
        Line =:= undefined orelse ?CHECK_SHIP_X(Line, Y)
    end, true, lists:seq(0, Size)).


%% @private
empty_map() ->
  EmptyLine = <<<<0>> || <<_:1>> <= <<0:10>>>>,
  lists:foldl(fun(N, Acc) -> Acc#{N => EmptyLine} end, #{}, lists:seq(0, 9)).

%% @private
get_horizont_line(Y, _) when Y < 0; Y > 10 -> undefined;
get_horizont_line(Y, Map) when is_map(Map)->
  #{Y := Line} = Map,
  Line.