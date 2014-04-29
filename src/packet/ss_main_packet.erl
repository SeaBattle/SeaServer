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
	{_, TypeInt, Protocol, Api, RawPacket} = ss_packet_header_pb:decode_header(Binary),
	{Type, Packet} = decode_body(TypeInt, RawPacket),
	{Type, Protocol, Api, Packet}.

%% guest_auth -- 1 (ss_auth_packet.proto)
%% login_auth -- 2 (ss_auth_packet.proto)
%% player_packet -- 3
%% error_packet -- 4
%% register_packet -- 5 (ss_auth_packet.proto)

% auth_packet
decode_body(TypeInt, Binary) when TypeInt == 1; TypeInt == 2 ->
	{Type, Uid, Password} = ss_auth_packet_pb:decode_auth_packet(Binary),
	{Type, {Uid, Password}}.

encode_packet(Type, {Code, Message}) when Type == error_packet ->
	Binary = ss_error_packet_pb:encode_error_packet({Type, Code, Message}),
	encode_header(Binary);
encode_packet(Type, #player{level = L, ban_type = BT, ban_end = BE, name = N, icon = I}) when Type == player_packet ->
	Binary = ss_player_packet_pb:encode_player_packet({L, BT, BE, N, I}),
	encode_header(Binary).

encode_header(Binary) ->
	Protocol = seaserver_app:get_conf_param(protocol, 1),
	MinApi = seaserver_app:get_conf_param(min_api, 1),
	ss_packet_header_pb:encode_header({header, 4, Protocol, MinApi, Binary}).