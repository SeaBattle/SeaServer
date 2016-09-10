%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2016 17:43
%%%-------------------------------------------------------------------
-module(ss_map_logic).
-author("tihon").

-include("ss_headers.hrl").
-include("ss_codes.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.
-define(PROPER(A, B), A >= 0, A =< 9, B >= 0, B =< 9).
-define(CHECK_SHIP_X(Line, X), 0 =:= binary:at(Line, X)).

%% API
-export([place_ships/2]).

place_ships(Ships, #{?NEAR_PLACING_HEAD := AllowNear}) ->
  EmptyMap = empty_map(),
  lists:foldl(fun(Ship, Map) -> place_ship(Ship, Map, AllowNear) end, EmptyMap, Ships).


%% @private
place_ship(#{?SHIP_ID_HEAD := Id, ?SHIP_X_POS_HEAD := X,
  ?SHIP_Y_POS_HEAD := Y, ?SHIP_DIRECTION_HEAD := D, ?SHIP_SIZE_HEAD := Size}, Map, Near) ->
  case can_be_placed(X, Y, D, Size, Map, Near) of
    true -> do_place_ship(Id, X, Y, D, Size, Map);
    false -> throw({error, ?INCORRECT_SHIP_POSITION})
  end.

%% @private
can_be_placed(X, Y, D, Size, Map, Near) when ?PROPER(X, Y) ->
  Cut = cut_space(Map, X, Y, Size, D, Near),
  check_cut(Cut);
can_be_placed(_, _, _, _, _, _) -> false.

%% @private
-spec cut_space(map(), integer(), integer(), integer(), binary(), boolean()) -> list() | binary().
cut_space(Map, X, Y, Size, <<"h">>, true) when X + Size =< 10 ->
  cut_horizontal(Size, X, [get_horizont_line(Y, Map)]);
cut_space(Map, X, Y, Size, <<"h">>, false) when X + Size =< 10 ->
  LineBefore = get_horizont_line(Y - 1, Map),
  LineCentral = get_horizont_line(Y, Map),
  LineAfter = get_horizont_line(Y + 1, Map),
  {UX, USize} = recalculate_coord(X, Size),
  cut_horizontal(USize, UX, [LineBefore, LineCentral, LineAfter]);
cut_space(Map, X, Y, Size, <<"w">>, true) when Y + Size =< 10 ->
  cut_vertical(X, Y, Size, Map, true);
cut_space(Map, X, Y, Size, <<"w">>, false) when Y + Size =< 10 ->
  {UY, USize} = recalculate_coord(Y, Size),
  cut_vertical(X, UY, USize, Map, false);
cut_space(_, _, _, _, _, _) -> [].

%% @private
cut_horizontal(Size, X, Lines) ->
  lists:foldl(
    fun
      ([], Acc) -> Acc;
      (Line, Acc) when is_binary(Line) ->
        [binary:part(Line, X, Size) | Acc]
    end, [], Lines).

%% @private
cut_vertical(X, Y, Size, Map, Near) ->
  R =
    lists:foldr(
      fun(N, Acc) ->
        case get_horizont_line(Y + N, Map) of
          Line when Near ->
            [get_horizontal_part(Line, X) | Acc];
          Line ->
            [compose_near(Line, X, Near) | [get_horizontal_part(Line, X) | Acc]]
        end
      end, [], lists:seq(0, Size - 1)),
  case Near of
    true -> R;
    false -> lists:flatten(R)
  end.

%% @private
compose_near(Line, X, false) ->
  [get_horizontal_part(Line, X - 1), get_horizontal_part(Line, X + 1)];
compose_near(_, _, true) -> [].

%% @private
check_cut([]) -> false;
check_cut(Bin) when is_binary(Bin) -> check_cut([Bin]);
check_cut(List) when is_list(List) ->
  0 =:= lists:foldl(fun sum_cut/2, 0, List).

%% @private
sum_cut([], Acc) -> Acc;
sum_cut(Bin, Acc) when is_binary(Bin) -> sum_cut(binary_to_list(Bin), Acc);
sum_cut(List, Acc) when is_list(List) -> lists:sum(List) + Acc;
sum_cut(Int, Acc) when is_integer(Int) -> Int + Acc.

%% @private
get_horizontal_part(Line, X) when X >= 0, X =< 9 -> binary:at(Line, X);
get_horizontal_part(_, _) -> [].

%% @private
recalculate_coord(N, Size) when N - 1 =:= -1 -> {0, incr(Size, 0, 1)};
recalculate_coord(N, Size) -> {N - 1, incr(Size, N - 1, 2)}.

%% @private
incr(S, C, N) when C + S + N > 10, N =:= 2 -> incr(S, C, 1);
incr(S, C, N) when C + S + N > 10, N =:= 1 -> S;
incr(S, _, N) -> S + N.

%% @private
null_or_more(0) -> 0;
null_or_more(N) -> N - 1.

%% @private
do_place_ship(Id, X, Y, <<"h">>, Size, Map) ->
  HorizontShip = get_horizont_line(Y, Map),
  UpdatedLine = ss_utils:replace_at(HorizontShip, X, <<<<Id>> || <<_:1>> <= <<0:Size>>>>),
  Map#{Y => UpdatedLine};
do_place_ship(Id, X, Y, <<"w">>, Size, Map) ->
  lists:foldl(
    fun(N, Acc) ->
      K = Y + N,
      Line = get_horizont_line(K, Acc),
      Acc#{K => ss_utils:replace_at(Line, X, <<Id>>)}
    end, Map, lists:seq(0, Size - 1)).

%% @private
empty_map() ->
  EmptyLine = <<<<0>> || <<_:1>> <= <<0:10>>>>,
  lists:foldl(fun(N, Acc) -> Acc#{N => EmptyLine} end, #{}, lists:seq(0, 9)).

%% @private
get_horizont_line(Y, _) when Y < 0; Y > 10 -> [];
get_horizont_line(Y, Map) when is_map(Map) ->
  #{Y := Line} = Map,
  Line.