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

%% API
-export([parse_header/1, parse_packet/1, send_packet/3]).

parse_header(Binary) ->
  {_, Type, Protocol, Api, Packet} = ss_packet_header_pb:decode_header(Binary),
  {Type, Protocol, Api, Packet}.

parse_packet(Binary) ->
  {TypeInt, Protocol, Api, RawPacket} = parse_header(Binary),
  {Type, Packet} = parse_body(TypeInt, RawPacket),
  {Type, Protocol, Api, {Packet}}.

% guest_auth_packet
parse_body(1, Binary) ->
  ss_guest_auth_pb:decode_guest_auth(Binary).

% error packet
send_packet(Type, Socket, {Code, Message}) when Type == error_packet ->
  Protocol = seaserver_app:get_conf_param(protocol, 1),
  MinApi = seaserver_app:get_conf_param(min_api, 1),
  Binary = ss_error_packet_pb:encode_error_packet({Type, Code, Message}),
  Packet = ss_packet_header_pb:encode_header({header, 4, Protocol, MinApi, Binary}),
  gen_tcp:send(Socket, Packet).