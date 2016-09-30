%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2016 10:40 AM
%%%-------------------------------------------------------------------
-module(ss_message_logic).
-author("tihon").

-include("ss_headers.hrl").
-include("ss_packet_type.hrl").

%% API
-export([encode/1, decode/1]).

-spec decode(binary()) -> {ok, map() | {error, any()}}.
decode(Binary) ->
  case msgpack:unpack(Binary) of
    {ok, Message} -> {ok, decode_message_type(Message)};
    Error -> Error
  end.

-spec encode(map()) -> binary() | {error, any()}.
encode(Map) ->
  msgpack:pack(encode_message_type(Map)).


%% @private
decode_message_type(Message = #{?PACKET_TYPE := TypeInt}) ->
  Message#{?PACKET_TYPE => lists:keyfind(TypeInt, 2, ?PACKAGES)}.

%% @private
encode_message_type(Message = #{?PACKET_TYPE := Type}) ->
  Message#{?PACKET_TYPE => lists:keyfind(Type, 1, ?PACKAGES)}.