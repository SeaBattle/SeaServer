%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. мар 2014 10:59
%%%-------------------------------------------------------------------
-module(ss_auth_man).
-author("tihon").

%% API
-export([make_auth/2]).

make_auth(1, {Uid}) -> ok;
make_auth(2, {Login, Password}) -> ok.