%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2016 18:17
%%%-------------------------------------------------------------------
-module(ss_handler_logic).
-author("tihon").

-include("ss_codes.hrl").
-include("ss_headers.hrl").
-include("ss_packet_type.hrl").
-include("ss_user.hrl").

-define(AUTH_CHECK, #{?PACKET_TYPE := ?AUTH_PACKET, ?UID_HEAD := _, ?TOKEN_HEAD := _}).
-define(CREATE_GAME_CHECK, #{?PACKET_TYPE := ?GAME_CREATE_PACKET, ?RULES_HEAD := _}).
-define(GAME_INVITE_CHECK, #{?PACKET_TYPE := ?GAME_INVITE_PACKET, ?GAME_ID_HEAD := _, ?UID_HEAD := _, ?RULES_HEAD := _}).
-define(GAME_ACCEPT_CHECK, #{?PACKET_TYPE := ?GAME_ACCEPT_PACKET, ?GAME_ID_HEAD := _, ?RESULT_HEAD := _, ?UID_HEAD := _}).
-define(GAME_JOIN_CHECK, #{?PACKET_TYPE := ?GAME_JOIN_PACKET, ?GAME_ID_HEAD := _}).
-define(SHIPS_CHECK, #{?PACKET_TYPE := ?GAME_SEND_SHIPS, ?SHIPS_HEAD := _}).
-define(FIRE_CHECK, #{?PACKET_TYPE := ?GAME_FIRE, ?FIRE_TYPE_HEAD := _, ?SHOT_HEAD := _}).
-define(FAST_PLAY_CHECK, #{?PACKET_TYPE := ?GAME_FAST_PLAY, ?VERSION_HEAD := _}).

%% API
-export([process_package_from_client/2, process_package_to_client/2]).

process_package_from_client(Package, UserState) ->
  case check_package(Package, UserState) of
    Err = {error, ?BAD_PACKAGE} -> Err;
    Fun -> do_process_package(maps:get(?PACKET_ID, Package, undefined), Fun, self())
  end.

-spec process_package_to_client(pid(), map()) -> tuple().
process_package_to_client(Client, Package) when is_pid(Client) ->
  Client ! {package, Package}.

%% @private
check_package(Packet = ?AUTH_CHECK, UserState) ->
  fun() -> ss_auth_man:auth_user(Packet, UserState) end;
check_package(Packet = ?CREATE_GAME_CHECK, UserState = #user_state{auth = true}) ->
  fun() -> ss_game_man:create_game(Packet, UserState) end;
check_package(Packet = ?GAME_INVITE_CHECK, UserState = #user_state{auth = true}) ->
  fun() -> ss_game_man:invite_game(Packet, UserState) end;
check_package(Packet = ?GAME_ACCEPT_CHECK, UserState = #user_state{auth = true}) ->
  fun() -> ss_game_man:accept_game(Packet, UserState) end;
check_package(Packet = ?GAME_JOIN_CHECK, UserState = #user_state{auth = true}) ->
  fun() -> ss_game_man:join_game(Packet, UserState) end;
check_package(Packet = ?SHIPS_CHECK, UserState = #user_state{auth = true}) ->
  fun() -> ss_game_man:send_ships(Packet, UserState) end;
check_package(Packet = ?FIRE_CHECK, UserState = #user_state{auth = true}) ->
  fun() -> ss_game_man:fire(Packet, UserState) end;
check_package(Packet = ?FAST_PLAY_CHECK, UserState = #user_state{auth = true}) ->
  fun() -> ss_game_man:fast_play(Packet, UserState) end;
check_package(_, _) ->
  {error, ?BAD_PACKAGE}.

%% @private
do_process_package(MayBeId, Fun, Self) ->
  proc_lib:spawn_link(
    fun() ->
      Result =
        try Fun()
        catch
          _:_Error ->
            #{?PACKET_TYPE => ?ERROR_PACKET, ?CODE_HEAD => ?SERVER_ERROR}
        end,
      return_result(Self, Result, MayBeId)
    end).

%% @private
return_result(Worker, Result, undefined) ->
  Worker ! {callback, Result},
  ok;
return_result(Worker, Result, MayBeId) ->
  Worker ! {callback, Result#{?PACKET_ID => MayBeId}},
  ok.