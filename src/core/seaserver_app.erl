-module(seaserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, get_conf_param/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ss_super_sup:start_link().

stop(_State) ->
    ok.

get_conf_param(Name, Default) ->
	try
		{ok, Application} = application:get_application(),
		application:get_env(Application, Name, Default)
	catch
		_:_ -> Default
	end.