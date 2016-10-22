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
-include("ss_user.hrl").
-include("ss_headers.hrl").
-include("ss_packet_type.hrl").

-define(DEFAULT_TTL, 5000). %5 sec

%% API
-export([fast_play/2, fire/2, send_ships/2, accept_game/2, invite_game/2, create_game/2, join_game/2]).

%% TODO allow fast play without rules and fetch rules from found game
%% TODO users should have some limit on not ended games (and should exit such games)
fast_play(Packet = #{?VERSION_HEAD := VSN}, US = #user_state{id = UID}) ->
  TTL = get_ttl(Packet),
  Rules = ss_rules_logic:encode_rules(Packet),
  Reply = case ss_game_service:fast_play(UID, VSN, Rules, TTL) of
            {true, GID, EUID, GRules} ->  %game was found
              start_game(UID, EUID, GID, Rules),
              #{?CODE_HEAD => ?OK, ?GAME_ID_HEAD => GID, ?UID_HEAD => EUID, ?RULES_HEAD => GRules};
            false ->  %will wait for the game (game will be created by other player join)
              #{?CODE_HEAD => ?GAME_NOT_READY}
          end,
  {Reply, US}.

%% register game creation in game service, get registered game_id if ok
create_game(Packet = #{?VERSION_HEAD := VSN}, US = #user_state{id = UID}) ->
  Rules = ss_rules_logic:encode_rules(Packet),
  Private = maps:get(?PRIVATE_HEAD, Packet, false),
  Reply = case ss_game_service:create_game(UID, VSN, Rules, Private) of
            {true, GID} ->  %game was created
              #{?CODE_HEAD => ?OK, ?GAME_ID_HEAD => GID};
            {false, Code} ->  %error creating game
              #{?CODE_HEAD => Code}
          end,
  {Reply, US}.

%% find online user and send him an invite
invite_game(Packet = #{?GAME_ID_HEAD := GID, ?UID_HEAD := EUID}, US = #user_state{id = UID}) ->
  Rules = ss_rules_logic:encode_rules(Packet),
  Package = #{?PACKET_TYPE => ?GAME_INVITE_PACKET, ?GAME_ID_HEAD => GID, ?UID_HEAD => UID, ?RULES_HEAD => Rules},
  Reply = case ss_messaging_logic:online_message(EUID, UID, Package) of %TODO offline invites sending
            true ->
              #{?CODE_HEAD => ?OK};
            false ->
              #{?CODE_HEAD => ?USER_OFFLINE}
          end,
  {Reply, US}.

%% accept|decline invitation to game
accept_game(#{?GAME_ID_HEAD := GID, ?RESULT_HEAD := false, ?UID_HEAD := EUID}, US = #user_state{id = UID}) -> %decline - just notify host
  Reply =
    case ss_messaging_logic:online_message(EUID, UID,
      #{?PACKET_TYPE => ?GAME_ACCEPT_PACKET, ?RESULT_HEAD => false, ?GAME_ID_HEAD => GID}) of
      true ->
        #{?CODE_HEAD => ?OK};
      false ->  %TODO offline invites sending
        #{?CODE_HEAD => ?USER_OFFLINE}
    end,
  {Reply, US};
accept_game(#{?GAME_ID_HEAD := GID, ?RESULT_HEAD := true}, US = #user_state{id = UID}) -> %accept - try to catch game and notify host
  Reply = case ss_game_service:join_game(GID, UID) of
            {true, CreatorUID, Rules} -> %joined the game
              case ss_messaging_logic:online_message(CreatorUID, UID,
                #{?PACKET_TYPE => ?GAME_ACCEPT_PACKET, ?RESULT_HEAD => false, ?GAME_ID_HEAD => GID}) of
                true -> %user notified. Create game.
                  start_game(UID, CreatorUID, GID, Rules),
                  #{?CODE_HEAD => ?OK};
                false ->  %TODO offline invites sending
                  #{?CODE_HEAD => ?USER_OFFLINE}
              end;
            {false, Code} ->  %can't join the game (no game or another player already join it)
              #{?CODE_HEAD => Code}
          end,
  {Reply, US}.

join_game(#{?GAME_ID_HEAD := GID}, US = #user_state{id = UID, self_pid = UserPid}) ->
  Reply = case find_game(GID, 2000) of
            undefined -> #{?CODE_HEAD => ?WRONG_GAME};
            {ok, Pid} ->
              case ss_game:try_join_game(Pid, UID, UserPid) of
                true -> #{?CODE_HEAD => ?OK};
                false -> #{?CODE_HEAD => ?WRONG_USER}
              end
          end,
  {Reply, US}.

send_ships(#{?GAME_ID_HEAD := GID, ?SHIPS_HEAD := Ships}, US) ->
  Reply = case find_game(GID, 2000) of
            undefined -> #{?CODE_HEAD => ?WRONG_GAME};
            {ok, Pid} -> %TODO find game in db?
              case ss_game:send_ships(Pid, Ships) of
                true -> #{?CODE_HEAD => ?OK};
                {error, Code} -> #{?CODE_HEAD => Code}
              end
          end,
  {Reply, US}.

fire(#{?GAME_ID_HEAD := GID, ?FIRE_HEAD := Fire}, US) ->
  Reply = case find_game(GID, 2000) of
            undefined -> #{?CODE_HEAD => ?WRONG_GAME};
            {ok, Pid} -> %TODO find game in db?
              case ss_game:fire(Pid, Fire) of
                true -> #{?CODE_HEAD => ?OK};
                {error, Code} -> #{?CODE_HEAD => Code}
              end
          end,
  {Reply, US}.


%% @private
-spec get_ttl(map()) -> integer().
get_ttl(#{?GAME_AWAIT_TTL_HEAD := TTL}) when is_integer(TTL) -> TTL;
get_ttl(_) -> ?DEFAULT_TTL.

%% @private
start_game(Uid, EUID, GID, Rules) ->
  {ok, _} = ss_game_sup:start_game(GID, Uid, EUID, Rules).

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