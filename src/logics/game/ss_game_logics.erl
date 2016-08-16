%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:22
%%%-------------------------------------------------------------------
-module(ss_game_logics).
-author("tihon").

-include("ss_headers.hrl").

-define(DEFAULT_TTL, 5000). %5 sec

%% API
-export([fast_play/1, do_fire/1, send_ships/1, reject_game/1, join_game/1, create_game/1]).

fast_play(Packet = #{?VERSION_HEAD := _Vsn, ?UID_HEAD := _UID}) ->
  _TTL = get_ttl(Packet),
  _RulesKey = ss_game_rules:form_rules(Packet),
  ok.

create_game(_Packet) ->
  ok.

join_game(_Packet) ->
  ok.

reject_game(_Packet) ->
  ok.

send_ships(_Packet) ->
  ok.

do_fire(_Packet) ->
  ok.


%% @private
-spec get_ttl(map()) -> integer().
get_ttl(#{?GAME_AWAIT_TTL_HEAD := TTL}) when is_integer(TTL) -> TTL;
get_ttl(_) -> ?DEFAULT_TTL.