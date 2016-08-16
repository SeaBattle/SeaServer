%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:23
%%%-------------------------------------------------------------------
-module(ss_auth_logic).
-author("tihon").

-include("ss_codes.hrl").
-include("ss_headers.hrl").
-include_lib("seaconfig/include/sc_headers.hrl").

-define(REGISTER_PATH, "/register").
-define(LOGIN_PATH, "/login").

%% API
-export([auth_user/1, register_user/1]).

auth_user(Packet = #{?UID_HEAD := Uid, ?SECRET_HEAD := Secret, ?USER_TOKEN := Token}) ->  %2 step
  ss_user_service:login(Uid, Secret, Token),
  case ss_service_logic:request_host("127.0.0.1", ?LOGIN_PATH, jsone:encode(Packet)) of
    Res = #{?RESULT_HEAD := true} ->
      %TODO set local user state = authed
      Res;
    Other ->
      Other
  end;
auth_user(Packet = #{?UID_HEAD := Uid}) -> %1 step
  ss_user_service:login(Uid),
  ss_service_logic:request_host("127.0.0.1", ?LOGIN_PATH, jsone:encode(Packet)).

-spec register_user(map()) -> map().
register_user(Packet = #{?EMAIL_HEAD := Email}) ->
  ss_user_service:register(Email),  %TODO get real host
  ss_service_logic:request_host("127.0.0.1", ?REGISTER_PATH, jsone:encode(Packet)).