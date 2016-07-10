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

-include("ss_headers.hrl").
-include("ss_packet_type.hrl").

%% API
-export([process_package/1]).

process_package(Packet = #{?PACKET_TYPE := ?AUTH_PACKET}) ->
  ss_auth_logic:auth_user(Packet),  %TODO return
  ok;
process_package(Packet = #{?PACKET_TYPE := ?REGISTER_PACKET}) ->
  ss_auth_logic:register_user(Packet),
  ok;
process_package(#{?PACKET_TYPE := ?USER_PACKET}) ->
  ok;
process_package(#{?PACKET_TYPE := ?CHAT_MSG_PACKET}) ->
  ok;
process_package(#{?PACKET_TYPE := ?CHAT_ENTER_PACKET}) ->
  ok;
process_package(#{?PACKET_TYPE := ?CHAT_LEAVE_PACKET}) ->
  ok;
process_package(#{?PACKET_TYPE := ?GAME_INVITE_PACKET}) ->
  ok;
process_package(#{?PACKET_TYPE := ?GAME_START_PACKET}) ->
  ok.