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

-include("ss_user.hrl").
-include("ss_headers.hrl").

-define(DEFAULT_TTL, 5000). %5 sec

%% API
-export([fast_play/2, fire/2, send_ships/2, accept_game/2, invite_game/2, create_game/2]).

%% TODO allow fast play without rules and fetch rules from found game
%% TODO users should have some limit on not ended games (and should exit such games)
fast_play(Packet = #{?VERSION_HEAD := VSN}, US = #user_state{id = UID}) ->
  TTL = get_ttl(Packet),
  Rules = ss_rules_logic:encode_rules(Packet),
  Reply = case ss_game_service:fast_play(UID, VSN, Rules, TTL) of
            {true, GID, EUID, GRules} ->  %game is ready
              find_game(GID, 2000);
            false ->  %will wait for the game
              ok
          end,
  {Reply, US}.

create_game(_Packet, _US) ->
  ok.

invite_game(_Packet, _US) ->
  ok.

accept_game(_Packet, _US) ->
  ok.

send_ships(_Packet = #{?GAME_ID_HEAD := _GID, ?SHIPS_HEAD := _Ships}, _US) ->
  ok.

fire(_Packet = #{?GAME_ID_HEAD := _GID}, _US) ->
  ok.


%% @private
-spec get_ttl(map()) -> integer().
get_ttl(#{?GAME_AWAIT_TTL_HEAD := TTL}) when is_integer(TTL) -> TTL;
get_ttl(_) -> ?DEFAULT_TTL.

%% @private
start_game(Uid, EUID, GID, Rules) ->
  {ok, Pid} = ss_game_sup:start_game(GID, Uid, EUID, Rules).

%% @private
%% Find created game by game id.
find_game(_, 0) -> undefined;
find_game(GID, Wait) ->
  case syn:find_by_key(GID) of
    undefined ->
      timer:sleep(100),
      find_game(GID, Wait - 100);
    Pid -> {ok, Pid}
  end.