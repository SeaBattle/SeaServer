%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2016 5:48 PM
%%%-------------------------------------------------------------------
-module(ss_websocket_handler).
-author("tihon").

-include("ss_headers.hrl").
-include("ss_user.hrl").
-include("ss_codes.hrl").
-include("ss_packet_type.hrl").

-define(MSGPACK, <<"msgpack">>).
-define(SUPPORTED_PROTOCOLS, [?MSGPACK]).
-define(DEFAULT_DISCONNECT, <<"15000">>). %15sec

%% API
-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-record(state,
{
  ping :: integer(),
  kill_timer :: timer:ref(),
  user_state :: #user_state{}
}).

init(Req, _) ->
  case is_websocket(Req) andalso supports_protocol(Req) of
    false ->
      Req2 = ss_notfound_handler:reply(Req),
      {ok, Req2, undefined};
    true ->
      Ping = binary_to_integer(?DEFAULT_DISCONNECT),  %TODO load from configuration
      Req2 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, <<"msgpack">>, Req),
      State = renew_timer(#state{ping = Ping}),
      {cowboy_websocket, Req2, State}
  end.

websocket_handle({binary, Msg}, Req, State = #state{user_state = US = #user_state{self_pid = undefined}}) ->
  process_message(Msg, Req, State#state{user_state = US#user_state{self_pid = self()}});
websocket_handle({binary, Msg}, Req, State) ->
  process_message(Msg, Req, State);
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({callback, {Msg, US}}, Req, State) ->
  Package = ss_message_logic:encode(Msg),
  {reply, {binary, Package}, Req, State = #state{user_state = US}}; %TODO recursive merge of old and new user state!
websocket_info({package, Msg}, Req, State) ->
  Package = ss_message_logic:encode(Msg),
  {reply, {binary, Package}, Req, State};
websocket_info(disconnect, Req, State) ->
  {stop, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.


%% @private
process_message(Binary, Req, State = #state{user_state = UserState}) ->
  UState = renew_timer(State),
  case ss_message_logic:decode(Binary) of
    {ok, Message} ->
      case ss_handler_logic:process_package_from_client(Message, UserState) of
        ok -> {ok, Req, UState};
        {error, Code} -> reply_error(Code, Req, UState)
      end;
    {error, _Error} ->
      reply_error(?BAD_PACKAGE, Req, UState)
  end.

%% @private
-spec supports_protocol(cowboy_req:req()) -> boolean().
supports_protocol(Req) ->
  case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
    undefined ->
      false;
    Protocols ->  % at least one client's supported protocols must be supported by the server
      [] /= lists:dropwhile(fun(Protocol) -> not lists:member(Protocol, ?SUPPORTED_PROTOCOLS) end, Protocols)
  end.

%% @private
-spec is_websocket(cowboy_req:req()) -> boolean().
is_websocket(Req) ->
  case cowboy_req:parse_header(<<"upgrade">>, Req) of
    undefined ->
      false;
    Protocols ->
      lists:member(<<"websocket">>, Protocols)
  end.

%% @private
renew_timer(State = #state{kill_timer = undefined, ping = TTL}) ->
  NewTimer = erlang:send_after(TTL, self(), disconnect),
  State#state{kill_timer = NewTimer};
renew_timer(State = #state{kill_timer = OldTimer, ping = TTL}) ->
  erlang:cancel_timer(OldTimer),
  NewTimer = erlang:send_after(TTL, self(), disconnect),
  State#state{kill_timer = NewTimer}.

%% @private
reply_error(Code, Req, State) ->
  BadPacket = #{?PACKET_TYPE => ?ERROR_PACKET, ?CODE_HEAD => Code},
  {reply, {binary, ss_message_logic:encode(BadPacket)}, Req, State}.