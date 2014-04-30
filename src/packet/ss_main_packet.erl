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

-include("ss_packet_header_pb.hrl").
-include("ss_guest_auth_pb.hrl").
-include("ss_records.hrl").

%% API
-export([decode_packet/1, encode_packet/2]).

decode_packet(Binary) ->
	{_, Type, Protocol, Api, RawPacket} = ss_packet_header_pb:decode_header(Binary),
	Packet = decode_body(Type, RawPacket),
	{Type, Protocol, Api, Packet}.

% auth_packet
decode_body(Type, Binary) when Type == guest_auth; Type == login_auth ->
	{_, Uid, Password} = ss_auth_packet_pb:decode_auth_packet(Binary),
	{Uid, Password}.

encode_packet(Type, {Code, Message}) when Type == error_packet ->
	Binary = ss_error_packet_pb:encode_error_packet({Type, Code, Message}),
	encode_header(Binary, Type);
encode_packet(Type, #player{level = L, ban_type = BT, ban_end = BE, name = N, icon = I}) when Type == player_packet ->
	BanEnd = if
		         BE == undefined -> 0;
		         true -> BE
	         end,
	Binary = ss_player_packet_pb:encode_player_packet({Type, L, BT, BanEnd, N, I}),
	encode_header(Binary, Type).

encode_header(Binary, Type) ->
	Protocol = seaserver_app:get_conf_param(protocol, 1),
	MinApi = seaserver_app:get_conf_param(min_api, 1),
	ss_packet_header_pb:encode_header({header, Type, Protocol, MinApi, Binary}).