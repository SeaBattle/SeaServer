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

-include_lib("seaconfig/include/sc_headers.hrl").

-define(DEFAULT_PROTOCOL, <<"4232">>).

%% API
-export([init/0]).

init() ->
  PortBin = sc_conf_holder:get_conf(?SEASERVER_HANDLER_TCP_PORT, ?DEFAULT_PROTOCOL),
  {ok, _} = ranch:start_listener(tcp_server, 100, ranch_tcp, [{port, binary_to_integer(PortBin)}], ss_tcp_handler, []).