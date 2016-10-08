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
-export([login/2]).

-spec login(binary(), binary()) -> boolean().
login(_Uid, _Token) ->
  ok.