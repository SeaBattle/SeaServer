%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2016 18:02
%%%-------------------------------------------------------------------
-module(ss_tcp_man).
-author("tihon").

%% API
-export([init/0]).

init() ->
  {ok, _} = ranch:start_listener(tcp_server, 100, ranch_tcp, [{port, 4232}], ss_tcp_handler, []).