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

%guest_auth_packet
parse_packet(<<Type:4/little-unit:8, ProtocolVersion:4/little-unit:8, ApiVersion:4/little-unit:8, Uid:30/bitstring>>) when Type == 1 ->
	{Type, ProtocolVersion, ApiVersion, {Uid}};
%login_auth_packet
parse_packet(<<Type:4/little-unit:8, ProtocolVersion:4/little-unit:8, ApiVersion:4/little-unit:8, Login:30/bitstring, Password:30/bitstring>>) when Type == 2 ->
	{Type, ProtocolVersion, ApiVersion, {Login, Password}}.