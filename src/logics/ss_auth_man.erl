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
-include_lib("seaconfig/include/sc_headers.hrl").

-define(REGISTER_PATH, "/register").
-define(LOGIN_PATH, "/login").

%% API
-export([auth_user/2]).

auth_user(#{?UID_HEAD := Uid, ?SECRET_HEAD := Secret, ?USER_TOKEN := Token}, UserState) ->  %2 step
  Result = ss_user_service:login(Uid, Secret, Token),
  {Result, UserState#user_state{auth = Result}};
auth_user(#{?UID_HEAD := Uid}, UserState) -> %1 step
  true = ss_user_service:login(Uid),
  {true, UserState}.