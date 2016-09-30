%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:32
%%%-------------------------------------------------------------------
-module(ss_websocket_man).
-author("tihon").

%% API
-export([init/0]).

init() ->
  %TODO port from conf
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/game", ws_handler, []},
      {'_', ss_notfound_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]).