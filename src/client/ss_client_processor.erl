%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Обработчик пакетов.
%%%
%%% @end
%%% Created : 30. апр 2014 18:11
%%%-------------------------------------------------------------------
-module(ss_client_processor).
-author("tihon").

%% API
-export([process_packet/3, process_header/2]).

process_header(Socket, ApiVersion) ->
	MinApi = seaserver_app:get_conf_param(min_api, 1),
	if
		ApiVersion < MinApi ->
			ss_main_packet:send_error(Socket, 400, <<"Old api version, please upgrade your app.">>),
			error;
		true -> ok
	end.

process_packet(Socket, Type, Body) when Type == guest_auth; Type == login_auth ->
	case ss_auth_man:make_auth(Type, Body) of
		{ok, Player} -> %TODO здесь может возникнуть ошибка и её нужно будет поймать (или не нужно. Отключать игрока при ошибках сервера или перебрасывать на другой поток?)
			Response = ss_main_packet:encode_packet(player_packet, Player),
			gen_tcp:send(Socket, Response),
			{ok, Player};
		Error ->
			io:format("Got error = ~w~n", [Error]),
			ss_main_packet:send_error(Socket, Error),
			error
	end;
process_packet(Socket, Type, Body) when Type == register_packet ->
	io:format("Register packet: ~p~n", [Body]),
	ss_auth_man:register(Body),
	ok.