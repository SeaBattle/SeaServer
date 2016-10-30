%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:25
%%%-------------------------------------------------------------------
-module(ss_handler_man).
-author("tihon").

-define(TCP_PROTOCOL, <<"tcp">>).
-define(WEBSOCKET_PROTOCOL, <<"websocket">>).

%% API
-export([init/0]).

init() ->
  Protocols = ?TCP_PROTOCOL,  %TODO load from configuration
  Splitted = binary:split(Protocols, <<",">>),
  lists:foreach(fun init_protocol/1, Splitted).

%% @private
init_protocol(?TCP_PROTOCOL) ->
  ss_tcp_man:init();
init_protocol(?WEBSOCKET_PROTOCOL) ->
  ss_websocket_man:init();
init_protocol(Unknown) ->
  throw({<<"Unknown protocol">>, Unknown}).