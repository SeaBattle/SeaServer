%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:22
%%%-------------------------------------------------------------------
-module(ss_game_man).
-author("tihon").

-include("ss_codes.hrl").
-include("ss_headers.hrl").

-define(DEFAULT_TTL, 5000). %5 sec
-define(FAST_GAME_PATH, "/play").


%% API
-export([fast_play/1, do_fire/1, send_ships/1, reject_game/1, join_game/1, create_game/1]).

%% TODO allow fast play without rules and fetch rules from found game
%% TODO users should have some limit on not ended games (and should exit such games)
fast_play(Packet = #{?VERSION_HEAD := Vsn, ?UID_HEAD := UID}) ->  %TODO may be save uid in thread as a state is more secure?
  TTL = get_ttl(Packet),
  RulesKey = ss_rules_logic:encode_rules(Packet),
  Request = #{?GAME_AWAIT_TTL_HEAD => TTL, ?RULES_HEAD => RulesKey, ?UID_HEAD => UID, ?VERSION_HEAD => Vsn},
  case ss_service_logic:request_host("127.0.0.1", ?FAST_GAME_PATH, jsone:encode(Request)) of
    #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?GAME_ID_HEAD := _GID, ?UID_HEAD := _EUID, ?RULES_HEAD := _Rules} ->  %game found
      %TODO create game and notify other user of game was created
      ok;
    #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?GAME_ID_HEAD := _GID, ?UID_HEAD := _EUID} ->  %game found
      ok;
    #{?RESULT_HEAD := true, ?CODE_HEAD := ?WAITING_FOR_CONNECT} ->  %waiting for game (other player will start the game)
      ok
  end,
  ok.

create_game(_Packet) ->
  ok.

join_game(_Packet) ->
  ok.

reject_game(_Packet) ->
  ok.

send_ships(_Packet = #{?GAME_ID_HEAD := _GID, ?SHIPS := _Ships}) ->
  ok.

do_fire(_Packet) ->
  ok.


%% @private
-spec get_ttl(map()) -> integer().
get_ttl(#{?GAME_AWAIT_TTL_HEAD := TTL}) when is_integer(TTL) -> TTL;
get_ttl(_) -> ?DEFAULT_TTL.

%% @private
start_game(Uid, EUID, GID, Rules) ->
  {ok, Pid} = ss_game_sup:start_game(GID, Uid, EUID, Rules).