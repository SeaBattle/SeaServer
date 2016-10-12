%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2016 20:04
%%%-------------------------------------------------------------------
-module(ss_game_service).
-author("tihon").

-include("ss_codes.hrl").
-include("ss_headers.hrl").

%% API
-export([fast_play/4, create_game/4, join_game/2]).

-spec create_game(ss_types:uid(), binary(), ss_types:rules_compressed(), boolean()) ->
  {true, ss_types:gid()} | {false, ss_types:code()}.
create_game(UID, VSN, Rules, Private) ->
  Request = #{?PRIVATE_HEAD => Private, ?RULES_HEAD => Rules, ?UID_HEAD => UID, ?VERSION_HEAD => VSN},
  case ss_service_logic:request_host(<<"game_service">>, <<"/create_game">>, jsone:encode(Request)) of
    #{?RESULT_HEAD := true, ?GAME_ID_HEAD := GID} ->  %game created
      {true, GID};
    #{?RESULT_HEAD := false, ?CODE_HEAD := Code} ->  %waiting for game (other player will start the game)
      {false, Code}
  end.

-spec join_game(ss_types:gid(), ss_types:uid()) ->
  {true, ss_types:uid(), ss_types:rules_compressed()} | {false, ss_types:code()}.
join_game(GID, UID) ->
  Request = #{?GAME_ID_HEAD => GID, ?UID_HEAD => UID},
  case ss_service_logic:request_host(<<"game_service">>, <<"/join_game">>, jsone:encode(Request)) of
    #{?RESULT_HEAD := true, ?UID_HEAD := EUID, ?RULES_HEAD := Rules} ->  %joined game, got enemy uid
      {true, EUID, Rules};
    #{?RESULT_HEAD := false, ?CODE_HEAD := Code} ->  %can't connect to game (game is not available or smth else)
      {false, Code}
  end.

-spec fast_play(ss_types:uid(), binary(), ss_types:rules_compressed(), integer()) ->
  {true, ss_types:gid(), ss_types:uid(), binary()} | false.
fast_play(UID, VSN, Rules, TTL) ->
  Request = #{?GAME_AWAIT_TTL_HEAD => TTL, ?RULES_HEAD => Rules, ?UID_HEAD => UID, ?VERSION_HEAD => VSN},
  case ss_service_logic:request_host(<<"game_service">>, <<"/fast_play">>, jsone:encode(Request)) of
    #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?GAME_ID_HEAD := GID, ?UID_HEAD := EUID, ?RULES_HEAD := GRules} ->  %game found, own rules
      {true, GID, EUID, GRules};
    #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?GAME_ID_HEAD := GID, ?UID_HEAD := EUID} ->  %game found, user rules
      {true, GID, EUID, Rules};
    #{?RESULT_HEAD := true, ?CODE_HEAD := ?WAITING_FOR_CONNECT} ->  %waiting for game (other player will start the game)
      false   %%TODO make waiting sync?
  end.