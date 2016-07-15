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
-include_lib("seaconfig/include/sc_headers.hrl").
-include_lib("seaconfig/include/sc_common_headers.hrl").

-define(REQUEST_TIMEOUT, 5000).
-define(CONTENT_TYPE, "application/json").
-define(REGISTER_PATH, "/register").
-define(LOGIN_PATH, "/login").

%% API
-export([auth_user/1, register_user/1]).

auth_user(Packet = #{?UID_HEAD := _, ?SECRET_HEAD := _, ?USER_TOKEN := _}) ->  %2 step
  Host = get_random_host(),
  case request_host(Host, ?LOGIN_PATH, jsone:encode(Packet)) of
    Res = #{?RESULT_HEAD := true} ->
      %TODO set local user state = authed
      Res;
    Other ->
      Other
  end;
auth_user(Packet = #{?UID_HEAD := _}) -> %1 step
  Host = get_random_host(),
  request_host(Host, ?LOGIN_PATH, jsone:encode(Packet)).

-spec register_user(map()) -> map().
register_user(Packet = #{?EMAIL_HEAD := _}) ->
  Host = get_random_host(),
  request_host(Host, ?REGISTER_PATH, jsone:encode(Packet)).


%% @private
get_random_host() ->
  Hosts = sc_conf_holder:get_conf(?USER_SERVICE_HOSTS),
  su_utils:get_random_element(Hosts).

%% @private
request_host(undefined, _, _) -> #{?RESULT_HEAD => false, ?CODE_HEAD => ?SERVER_ERROR};
request_host(Host, Request, Body) ->
  case httpc:request(post, {Host ++ Request, [], ?CONTENT_TYPE, Body}, [?REQUEST_TIMEOUT], []) of
    {{_, 200, _}, _, Res} ->
      jsone:decode(Res, [{object_format, map}]);
    _Other -> #{?RESULT_HEAD => false, ?CODE_HEAD => ?SERVER_ERROR}
  end.