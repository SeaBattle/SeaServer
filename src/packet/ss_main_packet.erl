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

-include("ss_player_packet_pb.hrl").
-include("ss_auth_packet_pb.hrl").
-include("ss_error_packet_pb.hrl").
-include("ss_packet_header_pb.hrl").
-include("ss_guest_auth_pb.hrl").
-include("ss_records.hrl").

%% API
-export([decode_packet/1, encode_packet/2, send_error/3, send_error/2]).

send_error(Socket, {Code, Message}) -> send_error(Socket, Code, Message).
send_error(Socket, Code, Message) ->
	Packet = encode_packet(error_packet, {Code, Message}),
	gen_tcp:send(Socket, Packet).

decode_packet(Binary) ->
	Header = ss_packet_header_pb:decode_header(Binary),
	Packet = decode_body(Header#header.type, Header#header.packet),
	{Header#header.type, Header#header.protocol, Header#header.apiversion, Packet}.

% auth_packet
decode_body(Type, Binary) when Type == guest_auth; Type == login_auth ->
	Auth = ss_auth_packet_pb:decode_auth_packet(Binary),
	{Auth#auth_packet.uid, Auth#auth_packet.password};
decode_body(Type, Binary) when Type == register_packet ->
	Register = ss_auth_packet_pb:decode_register_packet(Binary),
	{{Register#register_packet.login, Register#register_packet.password, Register#register_packet.uid},
		{Register#register_packet.name, Register#register_packet.icon_url, Register#register_packet.motto}}.

encode_packet(Type, {Code, Message}) when Type == error_packet ->
	Binary = ss_error_packet_pb:encode_error_packet(#error_packet{code = Code, descr = Message}),
	encode_header(Binary, Type);
encode_packet(Type, #player{level = L, ban_type = BT, ban_end = BE, name = N, icon = I}) when Type == player_packet ->
	BanEnd = if
		         BE == undefined -> 0;
		         true -> BE
	         end,
	Binary = ss_player_packet_pb:encode_player_packet(#player_packet{level = L, ban_type = BT, ban_end = BanEnd, name = N, icon = I}),
	encode_header(Binary, Type).

encode_header(Binary, Type) ->
	Protocol = seaserver_app:get_conf_param(protocol, 1),
	MinApi = seaserver_app:get_conf_param(min_api, 1),
	ss_packet_header_pb:encode_header(#header{type = Type, protocol = Protocol, apiversion = MinApi, packet = Binary}).