%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. мар 2014 21:50
%%%-------------------------------------------------------------------
-module(ss_main_packet).
-author("tihon").

%% API
-export([get_type_by_packet/1, get_packet_by_type/1, send_packet/3]).
%TODO подумать на свежую голову, как это можно упростить. (Ввести стандарты и т.п.)
send_packet(Type, Socket, Packet) when is_tuple(Packet) ->
	send_packet(Type, Socket, tuple_to_list(Packet));
send_packet(Type, Socket, Packet) when is_list(Packet) ->
	F = fun(X, Acc) ->
		if is_integer(X) -> [<<X:8/unit:4>> | Acc];
			true -> [X | Acc]
		end
	end,
	List = lists:reverse(lists:foldl(F, [], Packet)),
	send_packet(Type, Socket, list_to_binary(List));
send_packet(Type, Socket, Packet) when is_binary(Packet) ->
	BinaryType = ss_main_packet:get_packet_by_type(Type),
	ServerProtocolVersion = seaserver_app:get_conf_param(protocol, 1),
	gen_tcp:send(Socket, <<BinaryType:8/unit:4, ServerProtocolVersion:8/unit:4, Packet/binary>>);
send_packet(Type, Socket, Packet) ->
	send_packet(Type, Socket, [Packet]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Получает атом тип пакета из целочисленного значения
%%
%% @end
%%--------------------------------------------------------------------
%TODO подумать над какой-нибудь структурой данных в виде дерева, чтобы не писать вручную для каждого пакета соответствие int-тип
get_type_by_packet(1) -> guest_auth_packet;
get_type_by_packet(2) -> login_auth_packet;
get_type_by_packet(3) -> auth_resp_packet;
get_type_by_packet(4) -> error_packet.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Получает целочисленное значение пакета из атома типа
%%
%% @end
%%--------------------------------------------------------------------
get_packet_by_type(guest_auth_packet) -> 1;
get_packet_by_type(login_auth_packet) -> 2;
get_packet_by_type(auth_resp_packet) -> 3;
get_packet_by_type(error_packet) -> 4.

