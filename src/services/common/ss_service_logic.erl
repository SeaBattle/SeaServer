%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2016 20:07
%%%-------------------------------------------------------------------
-module(ss_service_logic).
-author("tihon").

-include("ss_codes.hrl").
-include("ss_headers.hrl").

-define(REQUEST_TIMEOUT, 5000).
-define(CONTENT_TYPE, "application/json").


%% API
-export([request_host/3]).

request_host(undefined, _, _) -> #{?RESULT_HEAD => false, ?CODE_HEAD => ?SERVER_ERROR};
request_host(Service, Request, Body) ->
  case get_random_host(Service) of
    [] ->
      #{?RESULT_HEAD => false, ?CODE_HEAD => ?SERVER_ERROR};
    Host -> %TODO override request timeout
      case httpc:request(post, {Host ++ Request, [], ?CONTENT_TYPE, Body}, [?REQUEST_TIMEOUT], []) of
        {{_, 200, _}, _, Res} ->
          jsone:decode(Res, [{object_format, map}]);
        _Other -> #{?RESULT_HEAD => false, ?CODE_HEAD => ?SERVER_ERROR}
      end
  end.

%% @private
get_random_host(_Service) ->
  ok.