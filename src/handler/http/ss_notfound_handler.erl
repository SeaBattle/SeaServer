%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2016 5:51 PM
%%%-------------------------------------------------------------------
-module(ss_notfound_handler).
-author("tihon").

%% API
-export([init/2, reply/1]).

init(Req, Opts) ->
  Req2 = reply(Req),
  {ok, Req2, Opts}.

reply(Req) ->
  cowboy_req:reply(404, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"Not found">>, Req).