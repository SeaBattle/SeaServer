%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:23
%%%-------------------------------------------------------------------
-module(ss_auth_man).
-author("tihon").

-include("ss_codes.hrl").
-include("ss_headers.hrl").
-include("ss_user.hrl").

-define(REGISTER_PATH, "/register").
-define(LOGIN_PATH, "/login").

%% API
-export([auth_user/2]).

auth_user(#{?UID_HEAD := Uid, ?USER_TOKEN := Token}, UserState = #user_state{self_pid = SelfPid}) ->
  case ss_user_service:login(Uid, Token) of
    true ->
      ok = syn:register(Uid, SelfPid),
      {true, UserState#user_state{auth = true, id = Uid}};
    false ->
      {false, UserState}
  end.