%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. мар 2014 10:32
%%%-------------------------------------------------------------------
-module(ss_auth_packet).
-author("tihon").

%% API
-export([parse_packet/1]).

%guest_auth_packet type 1
parse_packet(<<Type:8/little-unit:4, ProtocolVersion:8/little-unit:4, ApiVersion:8/little-unit:4, Uid/bitstring>>)
	when Type == 1 ->
	{guest_auth_packet, ProtocolVersion, ApiVersion, {Uid}};
%login_auth_packet type 2
parse_packet(<<Type:8/little-unit:4, ProtocolVersion:8/little-unit:4, ApiVersion:8/little-unit:4, Login:1/binary-unit:30, Password:1/binary-unit:30>>)
	when Type == 2 ->
	{login_auth_packet, ProtocolVersion, ApiVersion, {Login, Password}};
%error packet type 4
parse_packet(<<Type:8/little-unit:4, ProtocolVersion:8/little-unit:4, ApiVersion:8/little-unit:4, Message/bitstring>>)
	when Type == 4 ->
	{error_packet, ProtocolVersion, ApiVersion, {Message}}.