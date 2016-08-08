-module(seaserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Ret = ss_super_sup:start_link(),
  ss_handler_man:init(),
%%  TODO find and connect other nodes
  syn:init(),
  Ret.

stop(_State) ->
  ok.