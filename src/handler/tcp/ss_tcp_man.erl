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

-define(DEFAULT_PORT, <<"4232">>).

%% API
-export([init/0]).

init() ->
  PortBin = ?DEFAULT_PORT,  %TODO load from configuration
  {ok, _} = ranch:start_listener(tcp_server, 100, ranch_tcp, [{port, binary_to_integer(PortBin)}], ss_tcp_handler, []).