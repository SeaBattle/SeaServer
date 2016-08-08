%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2016 20:03
%%%-------------------------------------------------------------------
-module(ss_user_service).
-author("tihon").

-include("ss_headers.hrl").

%% API
-export([register/1, login/1, login/3]).

register(Email) ->
  ok.

login(Uid) ->
  ok.

login(Uid, Token, Service) ->
  ok.